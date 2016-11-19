import java.util.ArrayList;

public class Frame {

    int x;
    int y;
    int z;
    String name;
    Block[][][] blocks;
    //ArrayList<ArrayList<ArrayList<Block>>> blocks;
    ArrayList<Object[]> joins;

    Frame(String name, int x, int y, int z) {

        this.x = x;
        this.y = y;
        this.z = z;
        this.name = name;
        joins = new ArrayList<>();

        blocks = new ArrayList<>();
        for (int i = 0; i < x; i++) {

            ArrayList<ArrayList<Block>> y_list = new ArrayList<>();
            for (int j = 0; j < y; j++) {
                
                ArrayList<Block> z_list = new ArrayList<>();
                for (int k = 0; k < z; k++)
                    z_list.add(new Block(this));
                
                y_list.add(z_list);
            }
            blocks.add(y_list);
        }
        Test.faceCheck(blocks);
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

        if (joins.size() > 0) {
            
            sb.append("\n\n\tJoined_Frames ").append("\n\t{\n");
            for (int i = 0; i < joins.size(); i++) {
                
                if (joins.get(i) != null)
                {
                    sb.append("\t\t").append(((Frame) joins.get(i)[0]).name);
                    sb.append("<").
                            append(((int[]) joins.get(i)[1])[0] + 1).append(",").
                            append(((int[]) joins.get(i)[1])[1] + 1).append(",").
                            append(((int[]) joins.get(i)[1])[2] + 1).append(">");
                    sb.append(joins.get(i)[2]);

                    sb.append("   <---->   ");

                    sb.append(((Frame) joins.get(i)[3]).name);
                    sb.append("<").
                            append(((int[]) joins.get(i)[4])[0] + 1).append(",").
                            append(((int[]) joins.get(i)[4])[1] + 1).append(",").
                            append(((int[]) joins.get(i)[4])[2] + 1).append(">");
                    sb.append(joins.get(i)[5]);
                    sb.append("\n");
                }
            }
            sb.append("\t}");
        }
        
        sb.append("\n}");
        return sb.toString();
    }
}