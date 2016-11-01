import java.util.HashMap;

public class Frame {

    private int x;
    private int y;
    private int z;
    String name;                          // Frame name
    Block[][][] blocks;                   // the actual blocks of the frame. total # of blocks is always x*y*z
    Object[][] joins;                     // an array of size x*y*z*6 where each element = {A, A_coord, A_face, B, B_coord, B_face} (i.e. the args to Join())
    int num_joins;                        // the number of joined_frames;
    HashMap<String, Frame> joined_frames; // i.e. a way to access only those frame that are attached to this frame

    public Frame(String name, int x, int y, int z) {

        this.x = x;
        this.y = y;
        this.z = z;
        this.name = name;
        blocks = new Block[x][y][z];

        num_joins = 0;                        // running total on how many entries are in the joins matrix/list
        joins = new Object[x * y * z * 6][6]; // max # of possible joins is always x*y*z*6 (i.e the max # of available faces)
        joined_frames = new HashMap<String, Frame>();

        /*
            There needs to be some logic in these loops to check where the "default" joined sides are

            For example, for a frame with 3 blocks stacked on top of each other:
                1) the top side of the bottom-most block and the bottom side of the middle block
                2) the top side of the middle-most block and the bottom side of the top-most block
            should be changed to false (unavailable for join).
         */
        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                for (int k = 0; k < z; k++)
                    blocks[i][j][k] = new Block(i, j , k, this);
            }
        }
    }

    // Return frame's Block at coordinates x, y, z, adjusted for 1-based index
    public Block Block(int x, int y, int z) {
        try {
            return blocks[x-1][y-1][z-1];
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new IllegalArgumentException("Block<" + x + ", " + y + ", " + z +"> does not exist");
        }
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("Frame ").append(name).append(" \n{\n");
        sb.append("\tBlock<x, y, z> | [E, W, N, S, F, B]");
        sb.append("\n\t---------------|-------------------\n");
        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                for (int k = 0; k < z; k++) {
                    sb.append("\t").append(blocks[i][j][k]);
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
                    sb.append("\t\t").append(((Block)joins[i][0]).parent.name);
                    sb.append("<").
                            append(((int[]) joins[i][1])[0] + 1).append(",").
                            append(((int[]) joins[i][1])[1] + 1).append(",").
                            append(((int[]) joins[i][1])[2] + 1).append(">");
                    sb.append(joins[i][2]);

                    sb.append("   <---->   ");

                    sb.append(((Block) joins[i][3]).parent.name);
                    sb.append("<").
                            append(((int[]) joins[i][4])[0] + 1).append(",").
                            append(((int[]) joins[i][4])[1] + 1).append(",").
                            append(((int[]) joins[i][4])[2] + 1).append(">");
                    sb.append(joins[i][5]);
                }
            }
            sb.append("\n\t}");
        }
        sb.append("\n}");
        return sb.toString();
    }

}
