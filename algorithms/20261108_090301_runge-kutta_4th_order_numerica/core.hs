module Core (
    ODE,
    rk4Step,
    rk4Integrate
) where

-- | Type alias for a first‑order ordinary differential equation dy/dt = f(t, y)
type ODE = Double -> Double -> Double

-- | Perform a single Runge‑Kutta 4th order step.
--   Given the derivative function @f@, current time @t@, current value @y@,
--   and step size @h@, returns the approximated value at @t+h@.
rk4Step :: ODE -> Double -> Double -> Double -> Double
rk4Step f t y h =
    let k1 = f t y
        k2 = f (t + h/2) (y + h*k1/2)
        k3 = f (t + h/2) (y + h*k2/2)
        k4 = f (t + h)   (y + h*k3)
    in y + h * (k1 + 2*k2 + 2*k3 + k4) / 6

-- | Integrate an ODE from @t0@ to @tEnd@ using a fixed step size @h@.
--   Returns a list of (time, value) pairs, including the initial state.
--   The final point is guaranteed to be @tEnd@; the last step may be
--   smaller than @h@ to land exactly on @tEnd@.
rk4Integrate :: ODE -> Double -> Double -> Double -> Double -> [(Double, Double)]
rk4Integrate f t0 y0 tEnd h
    | h <= 0    = error "Step size h must be positive"
    | t0 > tEnd = error "Initial time must be <= final time"
    | otherwise = go t0 y0
  where
    go t y
        | t >= tEnd = [(tEnd, yFinal)]
        | otherwise =
            let hStep = min h (tEnd - t)
                yNext = rk4Step f t y hStep
                tNext = t + hStep
            in (t, y) : go tNext yNext
    -- If the loop ends exactly at tEnd we already have the correct y;
    -- otherwise we need to compute the final value with a possibly reduced step.
    yFinal
        | t0 == tEnd = y0
        | otherwise  = let hLast = tEnd - lastT
                           yLast = lastY
                       in if hLast == 0 then yLast else rk4Step f lastT yLast hLast
    (lastT, lastY) = case reverse (go t0 y0) of
                        []          -> (t0, y0)
                        ((tt, yy):_) -> (tt, yy)
