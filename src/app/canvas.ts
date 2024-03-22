import Konva from 'konva';
import { Mentat } from './mentat.ts';

interface Point {
    x: number;
    y: number
}


const contourLookup: {[key: number]: [Point, Point] | null} = {
    0: null,
    1: [{x: -1, y: 0}, {x: 0, y: 1}],
    2: [{x: 0, y: 1}, {x: 1, y: 0}],
    3: [{x: 0, y: -1}, {x: 1, y: 0}],
    4: [{x: -1, y: 0}, {x: 0, y: -1}],
    5: [{x: -1, y: 0}, {x: 1, y: 0}],
    6: [{x: 0, y: 1}, {x: 1, y: -1}],
    7: [{x: -1, y: 0}, {x: 1, y: 0}],
    8: [{x: 0, y: 1}, {x: 0, y: -1}],
    9: [{x: -1, y: 1}, {x: 1, y: -1}],
    10: [{x: -1, y: -1}, {x: 1, y: 1}],
    11: [{x: -1, y: 0}, {x: 0, y: 1}],
    12: [{x: 0, y: 1}, {x: 1, y: 0}],
    13: [{x: 0, y: -1}, {x: 1, y: 0}],
    14: [{x: -1, y: 0}, {x: 0, y: -1}],
    15: null
};

function buildLinePoints(
    constraint: Mentat.MentatConstraint,
    varriables: Mentat.MentatVariables,
    functions: Mentat.MentatFunctions,
    gridWidth: number,
    gridHeight: number,
    nCellsW: number,
    nCellsH: number): number[] {
    
    let vertices: boolean[][] = []
    type CoordSysLambda = (w: number, h: number) => [number, number];
    const vertToCoord: CoordSysLambda = (w, h) => [(gridWidth / nCellsW) * w, (gridHeight / nCellsH) * h];
    
    type cstrLambda = (w: number, h: number) => boolean;
    const cstrSample: cstrLambda = (w , h) => {
        let [x, y] = vertToCoord(w, h);
        return constraint.left(varriables, functions, x, y) > constraint.right(varriables, functions, x, y);
    };


    for (let i = 0; i < nCellsW + 1; i++) {
        vertices[i] = [];
        for (let j = 0; j < nCellsH + 1; j++) {
            vertices[i][j] = cstrSample(i, j)
        }
    }

    let cells: number[][] = [];

    for (let i = 0; i < nCellsW; i++) {
        cells[i] = [];
        for (let j = 0; j < nCellsH; j++) {
            let cellEdges: [boolean, boolean, boolean, boolean] = [
                vertices[i][j],             // Top left
                vertices[i + 1][j],         // Top right
                vertices[i + 1][j + 1],     // Bottom right
                vertices[i][j + 1]          // Bottom left
            ];

            switch (cellEdges) {
                case [false, false, false, false]:
                    cells[i][j] = 0;
                    break;
                case [true, false, false, false]:
                    cells[i][j] = 1;
                    break;
                case [false, true, false, false]:
                    cells[i][j] = 2;
                case [false, false, true, false]:
                    cells[i][j] = 3;
                    break;
                case [false, false, false, true]:
                    cells[i][j] = 4;
                    break;
                case [true, true, false, false]:
                    cells[i][j] = 5;
                    break;
                case [false, true, true, false]:
                    cells[i][j] = 6;
                    break;
                case [false, false, true, true]:
                    cells[i][j] = 7;
                    break;
                case [true, false, false, true]:
                    cells[i][j] = 8;
                    break;
                case [true, false, true, false]:
                    cells[i][j] = 9;
                    break;
                case [false, true, false, true]:
                    cells[i][j] = 10;
                    break;
                case [false, true, true, true]:
                    cells[i][j] = 11;
                    break;
                case [true, false, true, true]:
                    cells[i][j] = 12;
                    break;
                case [true, true, false, true]:
                    cells[i][j] = 13;
                    break;
                case [true, true, true, false]:
                    cells[i][j] = 14;
                    break;
                case [true, true, true, true]:
                    cells[i][j] = 15;
                    break;
            }
        }

    }

    let linePoints: number[] = []

    for (let i = 0; i < nCellsW; i++) { 
        for (let j = 0; j < nCellsH; j++) {
            let cellVal = contourLookup[cells[i][j]]
            if (cellVal !== null) {
                let ctPt1 = cellVal[0];
                let ctPt2 = cellVal[1];
                let [pt1x, pt1y] = vertToCoord(i + ctPt1.x, j + ctPt1.y);
                let [pt2x, pt2y] = vertToCoord(i + ctPt2.x, j + ctPt2.y);
                linePoints.concat([pt1x, pt1y, pt2x, pt2y]);
            }
        }
    }

    return linePoints;
}

export class FaradayCanvas {
    #canvas: Konva.Stage
    #height: number
    #width: number
    #origin: Point

    constructor(canvasId: string, height: number, width: number) {
        this.#canvas = new Konva.Stage({
            container: canvasId,
            height: height,
            width: width
        });
        this.#height = height;
        this.#width = width;
        this.#origin = {x: this.#width / 2, y: this.#height / 2}
        this.#plotAxes();
    }

    getOrigin(): Point {
        return this.#origin;
    }


    #plotAxes(): void {
        let yLayer = new Konva.Layer();

        let yAxis = new Konva.Line({
            points: [this.#origin.x, this.#height / 2, this.#origin.x, -this.#height - 2],
            stroke: 'black',
            strokeWidth: 5
        });

        yLayer.add(yAxis);

        let xLayer = new Konva.Layer();
        let xAxis = new Konva.Line({
            points: [this.#width / 2, this.#origin.y, -this.#width / 2, this.#origin.y],
            stroke: 'black',
            strokeWidth: 5
        });

        xLayer.add(yAxis)

        this.#canvas.add(yLayer)
        this.#canvas.add(yLayer)
    }


    plotConstraint(constraint: Mentat.MentatConstraint): void {
        return;
    }

}
    
