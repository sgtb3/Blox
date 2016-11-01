public class Block {

    int[] coord;                                                          // ONE block, with COORDINATES <x,y,z>
    boolean[] open_faces;
    float[] face_colors;
    Frame parent;

    public Block(int x, int y, int z, Frame parent) {
        coord = new int[] {x, y, z};
        open_faces = new boolean[] { true, true, true, true, true, true }; // E, W, N, S, F, B
        face_colors = new float[]  { 0, 0, 0, 0, 0, 0 };
        this.parent = parent;
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("Block<");
        sb.append(coord[0] + 1).append(", ");
        sb.append(coord[1] + 1).append(", ");
        sb.append(coord[2] + 1).append("> | <");

        for (int i = 0; i < open_faces.length; i++) {
            if (open_faces[i])
                sb.append(1);
            else
                sb.append(0);
            if (i != open_faces.length - 1)
                sb.append(", ");
        }
        sb.append(">");
        return sb.toString();
    }
}
