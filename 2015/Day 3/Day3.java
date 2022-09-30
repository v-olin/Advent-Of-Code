import java.io.*;
import java.util.*;
import java.util.stream.*;

public class Day3 {
    public static void main(String[] args) {
        new Day3().solve();
    }

    public void solve() {
        String inputFile = "/input.txt";
        InputStream dataStream = Day3.class.getResourceAsStream(inputFile);
        String data = readFromInputStream(dataStream);
        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
        return;
    }

    public int part1(String data) {
        HashMap<Point, Integer> map = new HashMap<>();
        Point santa = new Point(0, 0);
        map.put(santa, 1);
        int dataLen = data.length();
        for (int i = 0; i < dataLen; i++) {
            santa = move(santa, data.charAt(i));
            leaveGift(map, santa);
        }
        return map.size();
    }

    public Point move(Point p, char c) {
        if (c == '^') {
            return new Point(p.x, p.y + 1);
        } else if (c == 'v') {
            return new Point(p.x, p.y - 1);
        } else if (c == '>') {
            return new Point(p.x + 1, p.y);
        } else {
            return new Point(p.x - 1, p.y);
        }
    }

    public void leaveGift(HashMap<Point, Integer> map, Point p) {
        if (map.containsKey(p)) {
            map.replace(p, map.get(p) + 1);
        } else {
            map.put(p, 1);
        }
    }

    public int part2(String s) {
        HashMap<Point, Integer> map = new HashMap<>();
        Point[] santas = { new Point(0, 0), new Point(0, 0) };
        map.put(santas[0], 1);
        int dataLen = s.length();
        for (int i = 0; i < dataLen; i++) {
            santas[i % 2] = move(santas[i % 2], s.charAt(i));
            leaveGift(map, santas[i % 2]);
        }
        return map.size();
    }

    class Point {
        final int x;
        final int y;
        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override
        public int hashCode() {
            return x*y;
        }
        @Override
        public boolean equals(Object o) {
            if (o == this) return true;
            if (!(o instanceof Point)) {
                return false;
            }
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }
    }

    private static String readFromInputStream(InputStream inputStream) {
        StringBuilder resultStringBuilder = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = br.readLine()) != null) {
                resultStringBuilder.append(line).append("\n");
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
        return resultStringBuilder.toString();
    }
}
