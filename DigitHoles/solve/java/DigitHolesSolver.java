import java.io.FileReader;
import java.io.FileWriter;

import java.util.List;
import java.util.ArrayList;

import org.topcoder.TopcoderReader;
import org.topcoder.TopcoderWriter;
import org.topcoder.TypeRef;

public class DigitHolesSolver {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
        int number = (Integer) reader.next(Integer.class);
        reader.close();

        DigitHoles solver = new DigitHoles();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
        writer.write(solver.numHoles(number));
        writer.close();
    } catch (Exception err) {
        err.printStackTrace(System.err);
    }
    }
}
