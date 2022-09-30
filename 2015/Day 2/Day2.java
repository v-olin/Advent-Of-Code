import java.io.*;
import java.util.*;
import java.util.stream.*;

public class Day2 {
    public static void main(String[] args) {
        new Day2().solve();
    }

    public void solve() {
        String inputFile = "input.txt";
        List<String> data = readData(inputFile);
        
        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
    }

    public int part1(List<String> data) {
        return data.stream().mapToInt(s -> parseGift(s).getSurfaceArea()).sum();
    }

    public int part2(List<String> data) {
        var gifts = data.stream().map(s -> parseGift(s)).collect(Collectors.toList());
        return gifts.stream().mapToInt(g -> g.getBowLength() + g.getPerimiter()).sum();
    }

    public List<String> readData(String file) {
        List<String> data = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = br.readLine()) != null) {
                data.add(line);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
        return data;
    }

    public Gift parseGift(String dims) {
        String[] parts = dims.split("x");
        int l = Integer.parseInt(parts[0]);
        int w = Integer.parseInt(parts[1]);
        int h = Integer.parseInt(parts[2]);
        return new Gift(l, w, h);
    }

    public class Gift {
        private int length;
        private int width;
        private int height;

        public Gift(int length, int width, int height) {
            this.length = length;
            this.width = width;
            this.height = height;
        }

        public int getSmallestSide() {
            return Math.min(length * width, Math.min(width * height, height * length));
        }

        public int getSurfaceArea() {
            return 2 * length * width + 2 * width * height + 2 * height * length + getSmallestSide();
        }

        public int getPerimiter() {
            return 2 * Math.min(length + width, Math.min(width + height, height + length));
        }

        public int getBowLength() {
            return length * width * height;
        }
    }
}