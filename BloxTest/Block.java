public class Block {

    boolean[] open_faces;
    private float[] face_colors;

    public Block(Frame parent) {
                                   //  E     W     N     S     F     B
        open_faces = new boolean[] { true, true, true, true, true, true }; 
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < open_faces.length; i++) {
            if (open_faces[i])
                sb.append(1);
            else
                sb.append(0);
            if (i != open_faces.length - 1)
                sb.append(", ");
        }
        sb.append("]");
        return sb.toString();
    }
}
