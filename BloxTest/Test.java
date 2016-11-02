/*
 * This class should really be named "Compiler" since all the functions except main are all compiler functions.
 */

import java.util.Objects;
import java.util.ArrayList;


public class Test {

    public static void main(String[] args) {

        Frame A = new Frame("A", 1,1,3); // 3 blocks on top of each other
        Frame B = new Frame("B", 1,3,1); // 3 blocks next to each other
        Frame C = new Frame("C", 2,2,2); // a 2x2x2 square box

        System.out.println("\nNewly created frames: \n");
        System.out.println(A + "\n" + B + "\n" + C + "\n\n");

        /*
            A.Block(1,1,2) : The pointer to the 2nd block (from bottom) of Frame A
            coord(1,1,2)   : The coordinates of the 2nd block of Frame A
            "W"            : The side of the 2nd block of Frame A where the join will happen
            B.Block(1,1,1) : The pointer to the 1st (leftmost) block of Frame B
            coord(1,1,1)   : The coordinates of the 1st block of Frame B
            "E"            : The side of the 1st block of Frame B where the join will happen
         */
        Join(A, coord(1, 1, 2), "E", B, coord(1, 1, 1), "W");
        System.out.println("Frames A and B after Joining B<1,1,1>W to A<1,1,2>E ...\n");
        System.out.println(A +"\n");
        System.out.println(B +"\n");
        Join(A, coord(1, 1, 2), "W", C, coord(2, 2, 2), "E");
        System.out.println("Frames A and C after Joining C<2,2,2>E to A<1,1,2>W ...\n");
        System.out.println(A +"\n");
        System.out.println(C +"\n");

    }

    // Helper to fix the 1-based array index issue
    private static int[] coord(int x, int y, int z) {
        return new int[] {x-1, y-1, z-1};
    }

    // TODO: Implement this??
    private static Frame Detach(Object[] join_entry) {
        Frame f;

        return null;
    }


    private static ArrayList<ArrayList<ArrayList<Block>>> faceCheck(ArrayList<ArrayList<ArrayList<Block>>> A) {

        int x = 0;
        int y = 0;
        int z = 0;
        for (ArrayList<ArrayList<Block>> ylist : A) {
            y = 0;
            for (ArrayList<Block> zlist : ylist) {
                z = 0;
                for (Block block : zlist) {
                    if(block != null) {
                        Block xBlock = null;
                        Block yBlock = null;
                        Block zBlock = null;
                        if (x < (A.size() - 1)) {
                            xBlock = A.get(x + 1).get(y).get(z);
                        }
                        if (y < (ylist.size() -1)) {
                            yBlock = A.get(x).get(y + 1).get(z);
                        }
                        if (z < (zlist.size() - 1)) {
                            zBlock = A.get(x).get(y).get(z + 1);
                        }
                        if (xBlock != null) {
                            block.open_faces[0] = false;
                            xBlock.open_faces[1] = false;
                        }
                        if (yBlock != null) {
                            block.open_faces[2] = false;
                            yBlock.open_faces[3] = false;
                        }
                        if (zBlock != null) {
                            block.open_faces[4] = false;
                            zBlock.open_faces[5] = false;
                        }
                    }
                    z++;
                }
                y++;
            }
            x++;
        }
        return A;
    }


    private static void Join(Frame A, int[] A_coord, String A_face,
                             Frame B, int[] B_coord, String B_face) {

        int Ax = A_coord[0];
        int Ay = A_coord[1];
        int Az = A_coord[2];

        int Bx = B_coord[0];
        int By = B_coord[1];
        int Bz = B_coord[2];

        int Ax_shift = 0;
        int Ay_shift = 0;
        int Az_shift = 0;

        int Bx_shift = 0;
        int By_shift = 0;
        int Bz_shift = 0;

        int Cx_max = 0;
        int Cy_max = 0;
        int Cz_max = 0;


        // check if blocks are part of same frame
        if (A == B)
            System.err.println("Error: Attempting to join blocks from the same Frame.");

        // needed for next check
        boolean Aface = false;
        if (Objects.equals(A_face, "E"))
            Aface = A.blocks.get(Ax).get(Ay).get(Az).open_faces[0];
        else if (Objects.equals(A_face, "W"))
            Aface = A.blocks.get(Ax).get(Ay).get(Az).open_faces[1];
        else if (Objects.equals(A_face, "N"))
            Aface = A.blocks.get(Ax).get(Ay).get(Az).open_faces[2];
        else if (Objects.equals(A_face, "S"))
            Aface = A.blocks.get(Ax).get(Ay).get(Az).open_faces[3];
        else if (Objects.equals(A_face, "F"))
            Aface = A.blocks.get(Ax).get(Ay).get(Az).open_faces[4];
        else if (Objects.equals(A_face, "B"))
            Aface = A.blocks.get(Ax).get(Ay).get(Az).open_faces[5];

        // Check which face is being joined to, set shift factor accordingly
        boolean Bface = false;
        if (Objects.equals(B_face, "E")) {
            Bface = B.blocks.get(Bx).get(By).get(Bz).open_faces[0];
            Bx_shift = (Ax - 1) - Bx;
            By_shift = Ay - By;
            Bz_shift = Az - Bz;
        } else if (Objects.equals(B_face, "W")) {
            Bface = B.blocks.get(Bx).get(By).get(Bz).open_faces[1];
            Bx_shift = (Ax + 1) - Bx;
            By_shift = Ay - By;
            Bz_shift = Az - Bz;
        } else if (Objects.equals(B_face, "N")) {
            Bface = B.blocks.get(Bx).get(By).get(Bz).open_faces[2];
            Bx_shift = Ax - Bx;
            By_shift = (Ay - 1) - By;
            Bz_shift = Az - Bz;
        } else if (Objects.equals(B_face, "S")) {
            Bface = B.blocks.get(Bx).get(By).get(Bz).open_faces[3];
            Bx_shift = Ax - Bx;
            By_shift = (Ay + 1) - By;
            Bz_shift = Az - Bz;
        } else if (Objects.equals(B_face, "F")) {
            Bface = B.blocks.get(Bx).get(By).get(Bz).open_faces[4];
            Bx_shift = Ax - Bx;
            By_shift = Ay - By;
            Bz_shift = (Az - 1) - Bz;
        } else if (Objects.equals(B_face, "B")) {
            Bface = B.blocks.get(Bx).get(By).get(Bz).open_faces[5];
            Bx_shift = Ax - Bx;
            By_shift = Ay - By;
            Bz_shift = (Az + 1) - Bz;
        }


        // check if A's block face is available
        if (!Aface )
            System.err.println("Error: Block face is not available for Join: " + A);

        // check if B's block face is available
        if (!Bface )
            System.err.println("Error: Block face is not available for Join: " + B);

        // check for opposite faces
        if (Objects.equals(A_face, "E") && !Objects.equals(B_face, "W") ||
            Objects.equals(A_face, "W") && !Objects.equals(B_face, "E"))
            System.err.println("Error: Illegal face option.");

        if (Objects.equals(A_face, "N") && !Objects.equals(B_face, "S") ||
            Objects.equals(A_face, "S") && !Objects.equals(B_face, "N"))
            System.err.println("Error: Illegal face option.");

        if (Objects.equals(A_face, "F") && !Objects.equals(B_face, "B") ||
            Objects.equals(A_face, "B") && !Objects.equals(B_face, "F"))
            System.err.println("Error: Illegal face option.");


        /* ========== ALL CHECKS PASSED. BEGIN JOIN PROCESS ========== */



        // create an entry to be placed in the "joins Object[][] matrix" (this is the join information)
        Object[] join_entry = {A, A_coord, A_face, B, B_coord, B_face};

        // increment # of frames joined to A
        A.num_joins++;
        // add the join entry into A's joins
        A.joins[A.num_joins] = join_entry;
        // add a pointer to B into A's hashmap of joined frames (so A can access B)
        A.joined_frames.put(B.name, B);

        // increment # of frames joined to B
        B.num_joins++;
        // add the join entry into B's joins
        B.joins[B.num_joins] = join_entry;
        // add a pointer to A into B's hashmap of joined frames (so B can access A)
        B.joined_frames.put(A.name, A);
        
        // Determine shift values for A and B
        if (Bx_shift < 0) {
            Ax_shift = -Bx_shift;
            Bx_shift = 0;
        }
        if (By_shift < 0) {
            Ay_shift = -By_shift;
            By_shift = 0;
        }
        if (Bz_shift < 0) {
            Az_shift = -Bz_shift;
            Bz_shift = 0;
        }

        // Determine size of new array
        Cx_max = (A.x + Ax_shift);
        if (Cx_max < (B.x + Bx_shift)) {
            Cx_max = (B.x + Bx_shift);
        }
        Cy_max = (A.y + Ay_shift);
        if (Cy_max < (B.y + By_shift)) {
            Cy_max = (B.y + By_shift);
        }
        Cz_max = (A.z + Az_shift);
        if (Cz_max < (B.z + Bz_shift)) {
            Cz_max = (B.z + Bz_shift);
        }

        // Create new empty array Cblocks
        ArrayList<ArrayList<ArrayList<Block>>> Cblocks;
        Cblocks = new ArrayList<>();
        for (int i = 0; i < Cx_max; i++) {
            ArrayList<ArrayList<Block>> y_list = new ArrayList<>();
            for (int j = 0; j < Cy_max; j++) {
                ArrayList<Block> z_list = new ArrayList<>();
                for (int k = 0; k < Cz_max; k++) {
                    z_list.add(null);
                }
                y_list.add(z_list);
            }
            Cblocks.add(y_list);
        }

        // Fill Cblocks with blocks from frame A
        int x = 0;
        int y = 0;
        int z = 0;
        for (ArrayList<ArrayList<Block>> ylist : A.blocks) {
            y = 0;
            for (ArrayList<Block> zlist : ylist) {
                z = 0;
                for (Block block : zlist) {
                    if(block != null) {
                        Cblocks.get(x + Ax_shift).get(y + Ay_shift).set((z + Az_shift), block);
                    }
                    z++;
                }
                y++;
            }
            x++;
        }

        // Fill Cblocks with values from frame B
        x = 0;
        for (ArrayList<ArrayList<Block>> ylist : B.blocks) {
            y = 0;
            for (ArrayList<Block> zlist : ylist) {
                z = 0;
                for (Block block : zlist) {
                    if(block != null) {
                        Cblocks.get(x + Bx_shift).get(y + By_shift).set((z + Bz_shift), block);
                    }
                    z++;
                }
                y++;
            }
            x++;
        }

        // Check Cblocks faces
        Cblocks = faceCheck(Cblocks);

        // Print Cblocks
        x = 0;
        for (ArrayList<ArrayList<Block>> ylist : Cblocks) {
            y = 0;
            for (ArrayList<Block> zlist : ylist) {
                z = 0;
                for (Block block : zlist) {
                    if(block != null) {
                        System.out.println("("+x+", "+y+", "+z+") = " + block + "\n");
                    } else {
                        System.out.println("("+x+", "+y+", "+z+") = empty \n");
                    }
                    z++;
                }
                y++;
            }
            x++;
        }

        // Update Frame A with Cblocks array to finish merge of B into A
        A.blocks = Cblocks;
        A.x = Cx_max;
        A.y = Cy_max;
        A.z = Cz_max;
        
    }
}
