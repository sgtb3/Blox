/*
 * This class should really be named "Compiler" since all the functions except main are all compiler functions.
 */

import java.util.Objects;

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
        Join(A.Block(1,1,2), coord(1, 1, 2), "E", B.Block(1,1,1), coord(1, 1, 1), "W");
        System.out.println("Frames A and B after Joining B<1,1,1>W to A<1,1,2>E ...\n");
        System.out.println(A +"\n");
        System.out.println(B +"\n");

    }

    // just a helper to fix the 1-based array index issue
    private static int[] coord(int x, int y, int z) {
        return new int[] {x-1, y-1, z-1};
    }

    /*
        This would be a compiler function and it wouldn't have to take all these args.
        TODO: If A.parent.num_joins or B.parent.num_joins != 0, then one or both of the frames have other attached frames
              that must also be checked to make sure nothing illegal happens. This should be easy since for any given
              frame, we can access all other attached frames by simply looking in the given frame's hashmap of frame pointers.
     */

    private static void Join(Block A, int[] A_coord, String A_face,
                             Block B, int[] B_coord, String B_face) {

        // check if blocks are part of same frame
        if (A.parent == B.parent)
            System.err.println("Error: Attempting to join blocks from the same Frame.");

        // needed for next check
        boolean Aface = false;
        if (Objects.equals(A_face, "E"))
            Aface = A.open_faces[0];
        else if (Objects.equals(A_face, "W"))
            Aface = A.open_faces[1];
        else if (Objects.equals(A_face, "N"))
            Aface = A.open_faces[2];
        else if (Objects.equals(A_face, "S"))
            Aface = A.open_faces[3];
        else if (Objects.equals(A_face, "F"))
            Aface = A.open_faces[4];
        else if (Objects.equals(A_face, "B"))
            Aface = A.open_faces[5];

        // needed for next check
        boolean Bface = false;
        if (Objects.equals(B_face, "E"))
            Bface = B.open_faces[0];
        else if (Objects.equals(B_face, "W"))
            Bface = B.open_faces[1];
        else if (Objects.equals(B_face, "N"))
            Bface = B.open_faces[2];
        else if (Objects.equals(B_face, "S"))
            Bface = B.open_faces[3];
        else if (Objects.equals(B_face, "F"))
            Bface = B.open_faces[4];
        else if (Objects.equals(B_face, "B"))
            Bface = B.open_faces[5];

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

        // save the x,y,z coordinates of A and B, needed for next check
        int Ax = A.coord[0];
        int Ay = A.coord[1];
        int Az = A.coord[2];
        int Bx = B.coord[0];
        int By = B.coord[1];
        int Bz = B.coord[2];

        // check for illegal join placement (B gets joined to A)
        if (Bx > Ax && By > Ay && Bz > Az)
            System.err.println("Error: Illegal Join placement");


        /* ========== ALL CHECKS PASSED. BEING JOIN PROCESS ========== */


        // create an entry to be placed in the "joins Object[][] matrix" (this is the join information)
        Object[] join_entry = {A, A_coord, A_face, B, B_coord, B_face};

        // increment # of frames joined to A
        A.parent.num_joins++;
        // add the join entry into A's joins
        A.parent.joins[A.parent.num_joins] = join_entry;
        // add a pointer to B into A's hashmap of joined frames (so A can access B)
        A.parent.joined_frames.put(B.parent.name, B.parent);

        // increment # of frames joined to B
        B.parent.num_joins++;
        // add the join entry into B's joins
        B.parent.joins[B.parent.num_joins] = join_entry;
        // add a pointer to A into B's hashmap of joined frames (so B can access A)
        B.parent.joined_frames.put(A.parent.name, A.parent);

        // mark the appropriate face as unavailable for both A and B.
        if (Objects.equals(A_face, "E")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[0] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[1] = false;

        } else if (Objects.equals(A_face, "N")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[2] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[3] = false;

        } else if (Objects.equals(A_face, "F")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[4] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[5] = false;

        } else if (Objects.equals(A_face, "W")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[1] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[0] = false;

        } else if (Objects.equals(A_face, "S")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[3] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[2] = false;

        } else if (Objects.equals(A_face, "B")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[5] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[4] = false;

        }
    }
}
