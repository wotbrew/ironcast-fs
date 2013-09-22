using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Xna.Framework;

namespace FIronCS
{
    public static class Geom
    {

        public static IEnumerable<Point> Flood(this Point p, Func<Point, bool> f)
        {
            Queue<Point> q = new Queue<Point>();
            HashSet<Point> l = new HashSet<Point>();
            q.Enqueue(p);
            l.Add(p);
            while (q.Count > 0)
            {
                var n = q.Dequeue();
                if (f(n))
                {
                    yield return n;
                    var up = new Point(n.X, n.Y-1);
                    var down = new Point(n.X, n.Y+1);
                    var left = new Point(n.X-1, n.Y);
                    var right = new Point(n.X+2, n.Y);

                    if (l.Add(up))
                        q.Enqueue(up);
                    if (l.Add(down))
                        q.Enqueue(up);
                    if (l.Add(left))
                        q.Enqueue(up);
                    if (l.Add(right))
                        q.Enqueue(up);
                }
            }
        }
        public static IEnumerable<Point> Rays(this Point p, int range, float precision = 1)
        {
            return RaysWhile(p, null, range, precision);
        }

        public static void RayBlit<T>(this Point p, T[,] a, T val, bool[,] pred, int range, int skip = 0, float precision = 1)
        {
            double x2;
            double y2;
            for (float i = 0; i < 360; i += precision)
            {
                x2 = Math.Cos(i * 0.01745);
                y2 = Math.Sin(i * 0.01745);

                double ox = p.X + 0.5;
                double oy = p.Y + 0.5;

                int tx = 0;
                int ty = 0;

                int skipCount = 0;

                for (int z = 0; z < range; z++)
                {
                    tx = (int)ox;
                    ty = (int)oy;

                    if (skip > 0 && skip == skipCount)
                    {
                        break;
                    }
                    else if (pred[tx, ty])
                    {
                        a[tx, ty] = val;
                    }
                    else if (skip == skipCount)
                    {
                        break;
                    }
                    else
                    {
                        a[tx, ty] = val;
                        skipCount++;
                    }

                    ox += x2;
                    oy += y2;
                }
            }
        }
        public static IEnumerable<Point> RaysWhile(this Point p, Func<Point, bool> pred, int range, float precision = 1)
        {
            return RaysWhileSkip(p, pred, range, 0, precision);
        }
        public static IEnumerable<Point> RaysWhileSkip(this Point p, Func<Point, bool> pred, int range, int skip = 0, float precision = 1)
        {
            HashSet<Point> result = new HashSet<Point>();

            double x2;
            double y2;
            for (float i = 0; i < 360; i += precision)
            {
                x2 = Math.Cos(i * 0.01745);
                y2 = Math.Sin(i * 0.01745);

                double ox = p.X + 0.5;
                double oy = p.Y + 0.5;

                int tx = 0;
                int ty = 0;

                int skipCount = 0;

                for (int z = 0; z < range; z++)
                {
                    tx = (int)ox;
                    ty = (int)oy;

                    var pt = new Point(tx, ty);

                    if (skip > 0 && skip == skipCount)
                    {
                        break;
                    }
                    else if (pred == null || pred(pt))
                    {
                        result.Add(pt);
                    }
                    else if (skip == skipCount)
                    {
                        break;
                    }
                    else
                    {
                        result.Add(pt);
                        skipCount++;
                    }

                    ox += x2;
                    oy += y2;
                }
            }

            return result;
        }

        public static IEnumerable<Point> Line(this Point a, Point b)
        {
            int dx = Math.Abs(b.X - a.X);
            int dy = Math.Abs(b.Y - a.Y);

            int sx = 0;
            int sy = 0;

            if (a.X < b.X) sx = 1; else sx = -1;
            if (a.Y < b.Y) sy = 1; else sy = -1;

            int err = dx - dy;

            while (true)
            {
                yield return a;
                if (a == b) break;
                int e2 = 2 * err;
                if (e2 > -dy)
                {
                    err -= dy;
                    a.X += sx;
                }
                if (e2 < dx)
                {
                    err += dx;
                    a.Y += sy;
                }
            }
        }

        public static IEnumerable<Point> Arc(this Point a, int radius, double start, double end)
        {

            //var xstart = a.X + radius * Math.Cos(start);
            //var ystart = a.Y - radius * Math.Sin(start);
            //var xend = a.X + radius * Math.Cos(end);
            //var yend = a.Y - radius * Math.Sin(end);

            //xstart = xstart.Apply(Math.Round);
            //ystart = ystart.Apply(Math.Round);
            //xend = xend.Apply(Math.Round);
            //yend = yend.Apply(Math.Round);

            //return Circle(a, radius).Where(p =>
            //    p.X >= xstart && p.Y >= ystart
            //    && p.X <= xend && p.Y <= yend);

            HashSet<Point> result = new HashSet<Point>();

            var r = radius;
            for (var i = start; i < end; i = i + 0.05)
            {
                var i_ = MathHelper.WrapAngle((float)i);
                var p = new Point((int)Math.Round((double)a.X + Math.Cos(i_) * r), (int)Math.Round((double)a.Y + Math.Sin(i_) * r));

                if (p.X > a.X + radius)
                    p.X--;
                if (p.X < a.X - radius)
                    p.X++;
                if (p.Y > a.Y + radius)
                    p.Y--;
                if (p.Y < a.Y - radius)
                    p.Y++;

                result.Add(p);
            }

            return result;
        }

        public static IEnumerable<Point> Circle(this Point a, int radius)//, float start, float end)
        {
            int f = 1 - radius;
            int ddF_x = 1;
            int ddF_y = -2 * radius;
            int x = 0;
            int y = radius;

            //var xstart = a.X + radius * Math.Cos(Math.PI / 180 * start);
            //var ystart = a.Y - radius * Math.Sin(Math.PI / 180 * start);
            //var xend = a.X + radius * Math.Cos(Math.PI / 180 * end);
            //var yend = a.Y - radius * Math.Sin(Math.PI / 180 * end);

            yield return new Point(a.X, a.Y + radius);
            yield return new Point(a.X, a.Y - radius);
            yield return new Point(a.X + radius, a.Y);
            yield return new Point(a.X - radius, a.Y);

            int x0 = a.X;
            int y0 = a.Y;

            while (x < y)
            {
                // ddF_x == 2 * x + 1;
                // ddF_y == -2 * y;
                // f == x*x + y*y - radius*radius + 2*x - y + 1;
                if (f >= 0)
                {
                    y--;
                    ddF_y += 2;
                    f += ddF_y;
                }
                x++;
                ddF_x += 2;
                f += ddF_x;
                yield return new Point(x0 + x, y0 + y);
                yield return new Point(x0 - x, y0 + y);
                yield return new Point(x0 + x, y0 - y);
                yield return new Point(x0 - x, y0 - y);
                yield return new Point(x0 + y, y0 + x);
                yield return new Point(x0 - y, y0 + x);
                yield return new Point(x0 + y, y0 - x);
                yield return new Point(x0 - y, y0 - x);
            }
        }

        public static IEnumerable<Point> FilledCircle(this Point a, int radius)
        {
            int f = 1 - radius;
            int ddF_x = 1;
            int ddF_y = -2 * radius;
            int x = 0;
            int y = radius;

            yield return new Point(a.X, a.Y + radius);
            yield return new Point(a.X, a.Y - radius);
            yield return new Point(a.X + radius, a.Y);
            yield return new Point(a.X - radius, a.Y);

            int x0 = a.X;
            int y0 = a.Y;

            while (x < y)
            {
                if (f >= 0)
                {
                    y--;
                    ddF_y += 2;
                    f += ddF_y;
                }
                x++;
                ddF_x += 2;
                f += ddF_x;
                foreach (var p in new Point(x0 + x, y0 + y).Line(new Point(x0 - x, y0 + y)))
                    yield return p;
                foreach (var p in new Point(x0 + x, y0 - y).Line(new Point(x0 - x, y0 - y)))
                    yield return p;
                foreach (var p in new Point(x0 + y, y0 + x).Line(new Point(x0 - y, y0 + x)))
                    yield return p;
                foreach (var p in new Point(x0 + y, y0 - x).Line(new Point(x0 - y, y0 - x)))
                    yield return p;
            }
        }
    }
}
