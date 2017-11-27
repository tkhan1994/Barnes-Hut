import System.Environment
import Data.List
import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import Debug.Trace
import Data.Function (on)

g = 6.67e-11
type Vec2 = (Double, Double)
data Body = Body Vec2 Vec2 Double (Color3 Double)

findForces :: Body -> Double -> Vec2 -> Vec2
findForces (Body (posx,posy) _ mass _) cmass (cposx, cposy) =
    let (diffx, diffy) = ((posx - cposx), (posy - cposy))
        radiusSquared = ((^2) (diffx)) + ((^2) (diffy))
        force = (g * mass * cmass) / radiusSquared
        radius = sqrt radiusSquared
    in  (((force *  diffx) / radius), ((force * diffy) / radius))

updateBody :: (Foldable f) => f Body -> Body -> Body
updateBody bodies (Body (posx,posy) vel mass clr) = 
        let bodiesList = foldr (:) [] bodies
            filterBodies = filter (\(Body (px, py) _ _ _) -> (px /= posx) || (py /= posy)) bodiesList
            forceComp = map (\body -> findForces body mass (posx, posy)) filterBodies
            (fx, fy) = (foldl (+) 0 (map fst forceComp), foldl (+) 0 (map snd forceComp))
            nvx = (fx / mass) + (fst vel)
            nvy = (fy / mass) + (snd vel)
        in Body (posx + nvx, posy + nvy) (nvx, nvy) mass clr
 
instance Foldable QT where
    foldr _ acc Nil = acc 
    foldr func acc (Leaf a) = func a acc
    foldr func acc (Internal _ _ (t1, t2, t3, t4)) = foldr func (foldr func (foldr func (foldr func acc t4) t3) t2) t1

getPos :: Body -> Vec2
getPos (Body (posx,posy) _ _ _) = (posx, posy)

getQuadrant :: Vec2 -> Vec2 -> Int
getQuadrant (posx, posy) (cx, cy)
    | (posx < cx) && (posy >= cy) = 1
    | (posx >= cx) && (posy >= cy) = 2
    | (posx < cx) && (posy < cy) = 3
    | (posx >= cx) && (posy < cy) = 4


data QT a = Internal Double a (QT a,QT a,QT a,QT a) | Leaf a | Nil
makeQT :: Vec2 -> Double -> (a->Vec2) -> ([a]->a) -> [a] -> (QT a)
makeQT _ _ _ _ bodies | length bodies == 0 = Nil
makeQT _ _ _ _ bodies | length bodies == 1 = Leaf (head bodies)
makeQT (cx, cy) radius getPos summarize bodies =
    let center = (cx, cy)
        q1 = filter (\body -> (getQuadrant (getPos body) center) == 1) bodies
        q2 = filter (\body -> (getQuadrant (getPos body) center) == 2) bodies
        q3 = filter (\body -> (getQuadrant (getPos body) center) == 3) bodies
        q4 = filter (\body -> (getQuadrant (getPos body) center) == 4) bodies
        half = radius / 2.0
    in  Internal radius (summarize bodies)
        (makeQT (cx - half, cy + half) half getPos summarize q1,
        makeQT (cx + half, cy + half) half getPos summarize q2, 
        makeQT (cx - half, cy - half) half getPos summarize q3,
        makeQT (cx + half, cy - half) half getPos summarize q4)


tick ::Double -> [Body] -> [Body]
tick radius bodies =
    let bh = BH predicate (makeQT (0,0) radius getPos summarize bodies)
    in fmap (updateBody bh) bodies

data BH a = BH (Double -> a -> Bool) (QT a)
instance Foldable BH where
    foldr _ acc (BH f Nil) = acc
    foldr func acc (BH f (Leaf a)) = func a acc
    foldr func acc (BH f (Internal radius body (t1, t2, t3, t4)))
        | f radius body == True = func body acc
        | otherwise = foldr func (foldr func (foldr func (foldr func acc (BH f t4)) (BH f t3)) (BH f t2)) (BH f t1)

summarize :: [Body] -> Body
summarize bodies = 
    let ls = map (\(Body (x,y) _ m _) -> ((x,y),m)) bodies
        tmass = foldl (+) 0 (map snd ls)
        cmx = (foldr (\((x,y),m) acc -> (x * m) + acc) 0 ls) / tmass
        cmy = (foldr (\((x,y),m) acc -> (y * m) + acc) 0 ls) / tmass
    in Body (cmx, cmy) (0,0) tmass (Color3 0 0 0)

predicate :: Double -> Body -> Bool
predicate radius body
    | quotient >= 0.5 = False
    | otherwise = True
    where (px, py) = getPos body
          cx = px - (radius / 2)
          cy = py - (radius / 2)
          distance = sqrt (((^2) cx) + ((^2) cy))
          quotient = radius / distance

main :: IO ()
main = do
    (_,args) <- getArgsAndInitialize
    stdin <- getContents
    uncurry (mainChoice args) (parseInput stdin)

mainChoice :: [String] -> Double -> [Body] -> IO ()
mainChoice (iter:_) r bodies = putStr $ applyNtimes r bodies (read iter)
mainChoice [] r bodies = do
    createWindow "Barnes Hut"
    windowSize $= Size 700 700
    bodiesRef <- newIORef bodies
    ortho2D (-r) r (-r) r
    displayCallback $= (display r bodiesRef)
    addTimerCallback 10 (timer r bodiesRef)
    mainLoop

applyNtimes :: Double -> [Body] -> Int -> String
applyNtimes r bodies n = (unlines.map show) (iterate (tick r) bodies !! n)

parseInput :: String -> (Double, [Body])
parseInput input = 
    let (cnt:r:bodies) = lines input
    in (read r, map read (take (read cnt) bodies))

dispBody :: Body -> IO ()
dispBody (Body (x,y) _ _ rgb) = color rgb >> vertex (Vertex2 x y)

display :: Double -> IORef [Body] -> IO ()
display r bodiesRef = do
    clear [ColorBuffer]
    bodies <- get bodiesRef
    renderPrimitive Points (mapM_ dispBody bodies)
    flush

timer :: Double -> IORef [Body] -> IO ()
timer r bodiesRef = do
    postRedisplay Nothing
    bodies <- get bodiesRef
    bodiesRef $= tick r bodies 
    addTimerCallback 10 (timer r bodiesRef)

instance Read Body where
    readsPrec _ input = 
        let (x:y:vx:vy:m:r:g:b:rest) = words input
        in (\str -> [(Body (read x,read y) (read vx,read vy) (read m) 
            (Color3 ((read r)/255) ((read g)/255) ((read b)/255)), 
            unwords rest)]) input

instance Show Body where
    show (Body (x,y) (vx,vy) _ _) =
        "x=" ++ show x ++ " y=" ++ show y ++ " vx=" ++ 
            show vx ++ " vy=" ++ show vy

