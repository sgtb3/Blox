import java.util.ArrayList;
import java.util.HashMap;

public class Frame {

    int x;
    int y;
    int z;
    String name;                                  // Frame name
    ArrayList<ArrayList<ArrayList<Block>>> blocks;// the actual blocks of the frame. total # of blocks is always x*y*z
    Object[][] joins;                             // an array of size x*y*z*6 where each element = {A, A_coord, A_face, B, B_coord, B_face} (i.e. the args to Join())
    int num_joins;                                // the number of joined_frames;
    HashMap<String, Frame> joined_frames;         // i.e. a way to access only those frame that are attached to this frame

    Frame(String name, int x, int y, int z) {

        this.x = x;
        this.y = y;
        this.z = z;
        this.name = name;

        num_joins = 0;                        // running total on how many entries are in the joins matrix/list
        joins = new Object[x * y * z * 6][6]; // max # of possible joins is always x*y*z*6 (i.e the max # of available faces)
        joined_frames = new HashMap<>();

        blocks = new ArrayList<>();
        for (int i = 0; i < x; i++) {
            ArrayList<ArrayList<Block>> y_list = new ArrayList<>();
            for (int j = 0; j < y; j++) {
                ArrayList<Block> z_list = new ArrayList<>();
                for (int k = 0; k < z; k++) {
                    z_list.add(new Block(this));
                }
                y_list.add(z_list);
            }
            blocks.add(y_list);
        }
        Test.faceCheck(blocks);
    }

    // Return frame's Block at coordinates x, y, z, adjusted for 1-based index
    Block Block(int x, int y, int z) {
        try {
            return blocks.get(x-1).get(y-1).get(z-1);
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new IllegalArgumentException("Block<" + x + ", " + y + ", " + z +"> does not exist");
        }
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("Frame ").append(name).append(" \n{\n");
        sb.append("\t<Block> : [E, W, N, S, F, B]");
        sb.append("\n\t----------------------------\n");

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                for (int k = 0; k < z; k++) {
                    sb.append("\t").append("<");
                    sb.append(i+1).append(",");
                    sb.append(j+1).append(",");
                    sb.append(k+1).append("> : ");
                    sb.append(blocks.get(i).get(j).get(k));
                    if (i + 1 != x || j + 1 != y || k + 1 != z)
                        sb.append("\n");
                }
            }
        }

        if (num_joins > 0) {
            sb.append("\n\n\tJoined_Frames ").append("\n\t{\n");
            for (int i = 0; i < joins.length; i++) {
                if (joins[i][0] != null)
                {
                    sb.append("\t\t").append(((Frame) joins[i][0]).name);
                    sb.append("<").
                            append(((int[]) joins[i][1])[0] + 1).append(",").
                            append(((int[]) joins[i][1])[1] + 1).append(",").
                            append(((int[]) joins[i][1])[2] + 1).append(">");
                    sb.append(joins[i][2]);

                    sb.append("   <---->   ");

                    sb.append(((Frame) joins[i][3]).name);
                    sb.append("<").
                            append(((int[]) joins[i][4])[0] + 1).append(",").
                            append(((int[]) joins[i][4])[1] + 1).append(",").
                            append(((int[]) joins[i][4])[2] + 1).append(">");
                    sb.append(joins[i][5]);
                    sb.append("\n");
                }
            }
            sb.append("\t}");
        }
        sb.append("\n}");
        return sb.toString();
    }
}
