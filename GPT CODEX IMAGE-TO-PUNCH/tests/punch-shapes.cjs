const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require('node:path').join(__dirname, '..', 'app.js'), 'utf8');
const sandbox = {};
vm.createContext(sandbox);
for (const name of ['punchArea','calculateHoleProperties','punchOutline','drawPunch','buildPunchDxf','curveValueAt','buildToneLut','adjustImageTone']) {
  const start = source.indexOf(`function ${name}(`);
  const end = source.indexOf('\nfunction ', start + 1);
  vm.runInContext(source.slice(start, end), sandbox);
}
const {punchArea, calculateHoleProperties, punchOutline, drawPunch, buildPunchDxf} = sandbox;
const tools = [
  {shape:'circle',width:4,height:4,toolIndex:0},
  {shape:'square',width:4,height:4,toolIndex:1},
  {shape:'slot',width:4,height:8,toolIndex:2},
  {shape:'rectangle',width:4,height:8,toolIndex:3}
].sort((a,b)=>punchArea(a)-punchArea(b));
const settings={useFixedSizes:true,fixedPunches:tools,minDiameter:100,maxDiameter:200};
for(const [gray,index] of [[255,0],[180,1],[90,2],[0,3]]) {
  assert.equal(calculateHoleProperties(gray,settings).toolIndex,index);
  assert.equal(calculateHoleProperties(255-gray,{...settings,invert:true}).toolIndex,index);
}
const rect={x:25,y:50,shape:'rectangle',width:4,height:8};
assert.equal(punchOutline(rect).length,4);
assert.deepEqual(JSON.parse(JSON.stringify(punchOutline(rect)[0])),{type:'LINE',x1:23,y1:46,x2:27,y2:46});
for(const [width,height] of [[4,10],[10,4]]) {
  const edges=punchOutline({x:25,y:50,shape:'slot',width,height});
  assert.equal(edges.filter(e=>e.type==='LINE').length,2);
  const arcs=edges.filter(e=>e.type==='ARC');
  assert.equal(arcs.length,2);
  assert.ok(arcs.every(e=>e.r===2 && e.end-e.start===180));
  // The two caps and tangents must preserve the overall dimensions.
  assert.equal(Math.max(...arcs.map(e=>e.x+e.r))-Math.min(...arcs.map(e=>e.x-e.r)),width);
  assert.equal(Math.max(...arcs.map(e=>e.y+e.r))-Math.min(...arcs.map(e=>e.y-e.r)),height);
}
assert.equal(punchOutline({x:0,y:0,shape:'slot',width:4,height:4})[0].type,'CIRCLE');
assert.equal(punchOutline({x:0,y:0,shape:'circle',width:0,height:0}).length,0);
let arcs=0,lines=0;
sandbox.ctx={save(){},restore(){},translate(){},scale(){},beginPath(){},closePath(){},fill(){},moveTo(){},lineTo(){lines++},arc(){arcs++}};
drawPunch(rect,2,0,100);
assert.equal(arcs,0);assert.equal(lines,4);
drawPunch({x:0,y:0,shape:'slot',width:4,height:10},2,0,100);
assert.equal(arcs,2);
const output=buildPunchDxf(tools.map(p=>({...p,x:25,y:50})));
const records=output.trim().split('\n');
const types=[];for(let i=0;i<records.length;i+=2)if(records[i]==='0')types.push(records[i+1]);
assert.equal(types.filter(t=>t==='CIRCLE').length,1);
assert.equal(types.filter(t=>t==='LINE').length,10);
assert.equal(types.filter(t=>t==='ARC').length,2);
assert.ok(output.endsWith('0\nEOF\n'));
assert.throws(()=>buildPunchDxf([{...rect,width:NaN}]));
const neutralTone={imageBrightness:0,imageContrast:0,levelBlack:0,levelGamma:1,levelWhite:255,curvePoints:[{x:0,y:0},{x:255,y:255}]};
const neutralLut=sandbox.buildToneLut(neutralTone);
assert.equal(neutralLut[0],0);assert.equal(neutralLut[128],128);assert.equal(neutralLut[255],255);
const clippedLut=sandbox.buildToneLut({...neutralTone,levelBlack:50,levelWhite:200});
assert.equal(clippedLut[50],0);assert.equal(clippedLut[200],255);
const invertedCurve=sandbox.buildToneLut({...neutralTone,curvePoints:[{x:0,y:255},{x:255,y:0}]});
assert.equal(invertedCurve[0],255);assert.equal(invertedCurve[255],0);
console.log('PASS: punch shapes, DXF output, Levels, and Curves tone mapping.');
