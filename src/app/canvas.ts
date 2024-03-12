
import { PaperScope, Path, Point, Color } from "paper";


function getGraph(canvas: HTMLCanvasElement): PaperScope {
    let graph = new PaperScope;
    graph.setup(canvas)
    return graph;
}


window.onload = function() {
    
    let canvas = document.getElementById("graphCanvas") as HTMLCanvasElement; 
    let graph = getGraph(canvas);

    let path = new Path();
    path.strokeColor = new Color("black");

    let start = new Point(100, 100);
    path.moveTo(start);
    path.lineTo(start.add([200, -50]));
    graph.activate();
}

