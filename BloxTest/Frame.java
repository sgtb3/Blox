import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class Frame {

    int x;
    int y;
    int z;
    String name;               // Frame name
    Block[][][] blocks;        // the actual blocks of the frame. total # of blocks is always x*y*z
    Object[][] joined_frames;  // an array of size x*y*z*6 where each element = {A, A_coord, A_face, B, B_coord, B_face} (i.e. the args to Join())
    int num_joins;             // the number of joined_frames;

    public Frame(String name, int x, int y, int z) {

        this.x = x;
        this.y = y;
        this.z = z;
        this.name = name;
        blocks = new Block[x][y][z];
        num_joins = 0;

        // the max # of possible joins is always x*y*z*6 (i.e the max # of available faces)
        joined_frames = new Object[x*y*z*6][6];

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                for (int k = 0; k < z; k++)
                    blocks[i][j][k] = new Block(i, j , k, this);
            }
        }
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("Frame ").append(name).append(" \n{\n");

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

            for (int i = 0; i < joined_frames.length; i++) {

                if (joined_frames[i][0] != null)
                {
                    sb.append("\t\t").append(((Block)joined_frames[i][0]).parent.name);
                    sb.append("<").
                            append(((int[]) joined_frames[i][1])[0]).append(",").
                            append(((int[]) joined_frames[i][1])[1]).append(",").
                            append(((int[]) joined_frames[i][1])[2]).append(">");
                    sb.append(joined_frames[i][2]);

                    sb.append(" --> ");

                    sb.append(((Block) joined_frames[i][3]).parent.name);
                    sb.append("<").
                            append(((int[]) joined_frames[i][4])[0]).append(",").
                            append(((int[]) joined_frames[i][4])[1]).append(",").
                            append(((int[]) joined_frames[i][4])[2]).append(">");
                    sb.append(joined_frames[i][2]);
                }

            }
            sb.append("\n\t}");
        }

        sb.append("\n}");
        return sb.toString();
    }

}
