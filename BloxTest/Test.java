import java.util.Objects;

public class Test {

    // Tester
    public static void main(String[] args) {

        Frame A = new Frame("A", 1,1,3);
        Frame B = new Frame("B", 1,3,1);
        Frame C = new Frame("C", 2,2,2);

        System.out.println("Newly created frames: \n");
        System.out.println(A + "\n");
        System.out.println(B + "\n");
        System.out.println(C + "\n\n");


        // do a join
        Join(A.blocks[0][0][0], new int[] {0,0,1}, "W", B.blocks[0][0][0], new int[] {0,0,0}, "E");

        System.out.println("Frames A and B after performing Joining B<1,1,1>W to A<1,1,1>E ...\n");
        System.out.println(A +"\n");
        System.out.println(B +"\n");

    }

    // This would be a compiler function and it wouldn't have to take all these args.
    public static void Join(Block A, int[] A_coord, String A_face,
                            Block B, int[] B_coord, String B_face) {

        // check if blocks are part of same frame
        if (A.parent == B.parent)
            System.err.println("Error: Attempting to join blocks from the same Frame.");


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

        // check if block face is available
        if (!Aface || !Bface )
            System.err.println("Error: One or both Block faces is not available for Join.");

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


        int Ax = A.coord[0];
        int Ay = A.coord[1];
        int Az = A.coord[2];

        int Bx = B.coord[0];
        int By = B.coord[1];
        int Bz = B.coord[2];

        // check for illegal join placement (B gets joined to A)
        if (Bx > Ax && By > Ay && Bz > Az)
            System.err.println("Error: Illegal Join placement");

        // else it's valid, do the join
        Object[] join_entry = {A, A_coord, A_face, B, B_coord, B_face};
        A.parent.num_joins++;
        A.parent.joined_frames[A.parent.num_joins] = join_entry;
        B.parent.num_joins++;
        B.parent.joined_frames[B.parent.num_joins] = join_entry;


        if (Objects.equals(A_face, "E")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[0] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[1] = false;

        } else if (Objects.equals(A_face, "N")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[2] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[3] = false;

        } else if (Objects.equals(A_face, "F")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[4] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[5] = false;

        }

        else if (Objects.equals(A_face, "W")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[1] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[0] = false;

        } else if (Objects.equals(A_face, "S")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[3] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[2] = false;

        } else if (Objects.equals(A_face, "B")) {
            A.parent.blocks[Ax][Ay][Az].open_faces[5] = false;
            B.parent.blocks[Bx][By][Bz].open_faces[4] = false;

        }

        /*
        for (int x = 0; x < Ax; x++) {
            for (int y = 0; y < Ay; y++) {
                for (int z = 0; z < Az; z++) {

                }
            }
        }
        */
    }
}
