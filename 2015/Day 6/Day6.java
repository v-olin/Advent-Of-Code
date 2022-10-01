import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.stream.*;

public class Day6 {
    public static void main(String[] args) {
        new Day6().solve();
    }

    public void solve() {
        String inputFile = "input.txt";
        List<String> data = readData(inputFile);
        List<Instruction> instructions = data.stream().map(s -> parseInstruction(s)).collect(Collectors.toList());
        System.out.println("Part 1: " + part1(instructions));
        System.out.println("Part 2: " + part2(instructions));
    }

    public int part1(List<Instruction> instrs) {
        int[][] lights = new int[1000][1000];
        int count = 0;
        for (Instruction instr : instrs) {
            for (int i = instr.x1; i <= instr.x2; i++) {
                for (int j = instr.y1; j <= instr.y2; j++) {
                    if (instr.type == Instruction.Type.ON) {
                        if (lights[i][j] == 0) {
                            lights[i][j] = 1;
                            count++;
                        }
                    } else if (instr.type == Instruction.Type.OFF) {
                        if (lights[i][j] == 1) {
                            lights[i][j] = 0;
                            count--;
                        }
                    } else if (instr.type == Instruction.Type.TOGGLE) {
                        lights[i][j] = 1 - lights[i][j];
                        if (lights[i][j] == 1) {
                            count++;
                        } else {
                            count--;
                        }
                    }
                }
            }
        }
        return count;
    }

    public int part2(List<Instruction> instrs) {
        int[][] lights = new int[1000][1000];
        int total = 0;
        for (Instruction instr : instrs) {
            for (int i = instr.x1; i <= instr.x2; i++) {
                for (int j = instr.y1; j <= instr.y2; j++) {
                    if (instr.type == Instruction.Type.ON) {
                        lights[i][j]++;
                        total++;
                    } else if (instr.type == Instruction.Type.OFF) {
                        if (lights[i][j] > 0) {
                            lights[i][j]--;
                            total--;
                        }
                    } else if (instr.type == Instruction.Type.TOGGLE) {
                        lights[i][j] += 2;
                        total += 2;
                    }
                }
            }
        }
        return total;
    }

    public Instruction parseInstruction(String instr) {
        String[] parts = instr.split(" ");
        int i1 = 2, i2 = 4;

        Instruction.Type type;
        if (parts.length == 5) {
            type = parts[1].equals("on") ? Instruction.Type.ON : Instruction.Type.OFF;
        } else {
            type = Instruction.Type.TOGGLE;
            i1 = 1;
            i2 = 3;
        }
        
        String[] coords1 = parts[i1].split(",");
        String[] coords2 = parts[i2].split(",");

        Instruction i = new Instruction(
            type,
            Integer.parseInt(coords1[0]),
            Integer.parseInt(coords1[1]),
            Integer.parseInt(coords2[0]),
            Integer.parseInt(coords2[1])
        );
        return i;
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

    static class Instruction {
        public enum Type {
            ON, OFF, TOGGLE
        }

        final Type type;
        final int x1, y1, x2, y2;

        public Instruction(Type type, int x1, int y1, int x2, int y2) {
            this.type = type;
            this.x1 = x1;
            this.y1 = y1;
            this.x2 = x2;
            this.y2 = y2;
        }
    }
}
