import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.stream.*;

public class Day7 {
    static HashMap<String, Node> wires = new HashMap<>();

    public static void main(String[] args) {
        new Day7().solve();
    }

    public void solve() {
        String file = "input.txt";
        List<String> instr = readData(file);

        System.out.println("Part 1: " + part1(instr));
        System.out.println("Part 2: " + part2(instr));
    }

    public int part1(List<String> instr) {
        for (String s : instr) {
            Node node = parseNode(s);
            wires.put(node.id, node);
        }

        return wires.get("a").eval() & 0xFFFF;
    }

    public int part2(List<String> instr) {
        wires.clear();

        for (String s : instr) {
            Node node = parseNode(s);
            wires.put(node.id, node);
        }
        
        Node b = wires.get("b");
        b.op = Node.Operator.DC;
        b.left = new Input(Integer.toString(3176));

        return wires.get("a").eval() & 0xFFFF;
    }

    public Node parseNode(String s) {
        String[] parts = s.split(" ");

        Node node = new Node(parts[parts.length - 1]);

        if (parts.length == 3) {
            node.op = Node.Operator.DC;
            node.left = new Input(parts[0]);
        } else if (parts[1].equals("AND")) {
            node.op = Node.Operator.AND;
            node.left = new Input(parts[0]);
            node.right = new Input(parts[2]);
        } else if (parts[1].equals("LSHIFT")) {
            node.op = Node.Operator.LSHIFT;
            node.left = new Input(parts[0]);
            node.right = new Input(parts[2]);
        } else if (parts[1].equals("RSHIFT")) {
            node.op = Node.Operator.RSHIFT;
            node.left = new Input(parts[0]);
            node.right = new Input(parts[2]);
        } else if (parts[1].equals("OR")) {
            node.op = Node.Operator.OR;
            node.left = new Input(parts[0]);
            node.right = new Input(parts[2]);
        } else if (parts[0].equals("NOT")) {
            node.op = Node.Operator.NOT;
            node.left = new Input(parts[1]);
        }

        return node;
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

    public class Input {
        private String value;
        private int cache;

        public Input(String value) {
            this.value = value;
        }

        public int getValue() {
            if (value.matches("[0-9]+")) {
                return Integer.parseInt(value);
            } else {
                cache = wires.get(value).eval();
                return cache;
            }
        }
    }

    static class Node {
        public enum Operator {
            DC,
            AND,
            LSHIFT,
            RSHIFT,
            NOT,
            OR,
            NONE
        }

        final String id;
        Operator op;
        Input left;
        Input right;
        int cache;

        public Node(String id) {
            this.id = id;
            this.op = Operator.NONE;
        }

        public int eval() {
            if (cache != 0)
                return cache;

            if (left == null && right == null)
                return 0;

            switch (op) {
                case DC:
                    cache = left.getValue();
                    break;
                case AND:
                    cache = left.getValue() & right.getValue();
                    break;
                case LSHIFT:
                    cache = left.getValue() << right.getValue();
                    break;
                case RSHIFT:
                    cache = left.getValue() >> right.getValue();
                    break;
                case NOT:
                    cache = ~left.getValue();
                    break;
                case OR:
                    cache = left.getValue() | right.getValue();
                    break;
                default:
                    cache = 0;
            }

            return cache;
        }
    }
}