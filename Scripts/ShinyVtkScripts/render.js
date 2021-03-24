function (x, global) {
  var el = global.el
  var width = global.width
  var height = global.height

  // pseudo imports (avoids having to use fully qualified names)
  var vtkFullScreenRenderWindow = vtk.Rendering.Misc.vtkFullScreenRenderWindow;
  var vtkImageData = vtk.Common.DataModel.vtkImageData;
  var vtkDataArray = vtk.Common.Core.vtkDataArray;
  var vtkVolume = vtk.Rendering.Core.vtkVolume;
  var vtkVolumeMapper = vtk.Rendering.Core.vtkVolumeMapper;
  var vtkColorTransferFunction = vtk.Rendering.Core.vtkColorTransferFunction;
  var vtkPiecewiseFunction = vtk.Common.DataModel.vtkPiecewiseFunction;
  var VtkDataTypes = vtkDataArray.VtkDataTypes;
  var vtkVolumeController = vtk.Interaction.UI.vtkVolumeController;
  var vtkActor = vtk.Rendering.Core.vtkActor;
  var vtkImageMarchingCubes = vtk.Filters.General.vtkImageMarchingCubes;
  var vtkMapper = vtk.Rendering.Core.vtkMapper;
  var vtkImageMapper = vtk.Rendering.Core.vtkImageMapper;
  var vtkImageSlice = vtk.Rendering.Core.vtkImageSlice;

  // A few helper functions
  function setVolumeProperties(property) {
    property.setInterpolationTypeToLinear();
    property.setAmbient(0.2);
    property.setDiffuse(0.7);
    property.setSpecular(0.3);
    property.setSpecularPower(8.0);
  }

  function setBackgroundProperties(props){
    props.setAmbient(0.1);
    props.setDiffuse(0.8);
    props.setSpecular(0.2);
    props.setSpecularPower(8.0);
    props.setBackfaceCulling(false);
    props.setInterpolationToPhong();
  }

  function setSliceProperties(props, range ){
    var colorLevel = 0.5*range[1]
    props.setColorLevel(colorLevel);
    props.setColorWindow(range[1]);
  }

  const renderWindow = global.windowRenderer.getRenderWindow();
  console.log(renderWindow)
  const renderer = global.windowRenderer.getRenderer();


  //Set up pipelines
  if( !global.initialized ){
    renderer.setBackground(0, 0, 0);

    //Set up volume rendering
    global.volumeMapper = vtkVolumeMapper.newInstance();
    global.volumeMapper.setSampleDistance(0.7);
    global.volumeActor = vtkVolume.newInstance();
    global.volumeActor.setMapper(global.volumeMapper);
    setVolumeProperties( global.volumeActor.getProperty() );

    //Set up 3D background
    global.actor = vtkActor.newInstance();
    setBackgroundProperties( global.actor.getProperty() );
    global.mapper = vtkMapper.newInstance();
    global.marchingCube = vtkImageMarchingCubes.newInstance({
      contourValue: x.data.isovalue,
      computeNormals: true,
      mergePoints: true,
    });
    global.mapper.setInputConnection(global.marchingCube.getOutputPort());
    global.actor.setMapper(global.mapper);


    //Set up slices
    global.imageActorX = vtkImageSlice.newInstance();
    global.imageMapperX = vtkImageMapper.newInstance();
    global.imageActorX.setMapper(global.imageMapperX);

    global.imageActorY = vtkImageSlice.newInstance();
    global.imageMapperY = vtkImageMapper.newInstance();
    global.imageActorY.setMapper(global.imageMapperY);

    global.imageActorZ = vtkImageSlice.newInstance();
    global.imageMapperZ = vtkImageMapper.newInstance();
    global.imageActorZ.setMapper(global.imageMapperZ);

    global.initalizeController = true;
    global.initialized = true;
  }

  if(x.data.image){
    var imageData = global.toVtkImageData(x.data.image);
    global.volumeMapper.setInputData(imageData);
    global.volumeMapper.modified();
    global.volumeActor.modified();
    renderer.addVolume(global.volumeActor);

    //First call with image data and pipelines initalized
    if( global.initalizeController ){
      global.initalizeController = false;

      global.controllerWidget = vtkVolumeController.newInstance({
        size: [400, 150],
        rescaleColorMap: true,
      });

      const isBackgroundDark = true;
      global.controllerWidget.setupContent( renderWindow, global.volumeActor,
                                            isBackgroundDark, true, 'PiYG');
      var widget = global.controllerWidget.getWidget();
      widget.removeGaussian(0);
      widget.addGaussian(0.2, 1, 0.25, -0.12, 0);
      widget.addGaussian(0.8, 1, 0.25,  0.12, 0);
      global.controllerWidget.setContainer(el);

      global.windowRenderer.setResizeCallback(({ width, height }) => {
        // 2px padding + 2x1px boder + 5px edge = 14
        if (width > 414) {
          global.controllerWidget.setSize(400, 150);
        } else {
         global.controllerWidget.setSize(width - 14, 150);
        }
        global.controllerWidget.render();
      });

      renderer.resetCamera();
      renderer.getActiveCamera().elevation(-70);
      renderer.updateLightsGeometryToFollowCamera();
    }
    //Update controller with new image data
    var widget = global.controllerWidget.getWidget();
    widget.setDataArray(imageData.getPointData().getScalars().getData() );
    global.controllerWidget.modified();
    global.controllerWidget.render();
  }

  if(x.data.background){
    global.marchingCube.setContourValue(x.data.isovalue);
    const bgImageData = global.toVtkImageData(x.data.background);
    global.marchingCube.setInputData(bgImageData);
    renderer.addActor(global.actor);

    global.imageMapperX.setInputData(bgImageData);
    global.imageMapperY.setInputData(bgImageData);
    global.imageMapperZ.setInputData(bgImageData);
    renderer.addActor(global.imageActorX);
    renderer.addActor(global.imageActorY);
    renderer.addActor(global.imageActorZ);

    const range = bgImageData.getPointData().getScalars().getRange();
    var colorLevel = 0.5*range[1]
    setSliceProperties(global.imageActorX.getProperty(), range)
    setSliceProperties(global.imageActorY.getProperty(), range)
    setSliceProperties(global.imageActorZ.getProperty(), range)
  }

  if( x.data.isovalue != global.marchingCube.getContourValue() ){
   global.marchingCube.setContourValue(x.data.isovalue);
   global.marchingCube.modified();
   global.mapper.modified();
  }
  var props = global.actor.getProperty()
  props.setOpacity(x.data.opacity);
  if(x.data.solid){
    props.setRepresentationToSurface();
  }
  else{
    props.setRepresentationToWireframe();
  }
  global.actor.setVisibility(x.data.showBackground)
  global.actor.modified();

  global.imageActorX.setVisibility(x.data.showSliceX)
  global.imageActorY.setVisibility(x.data.showSliceY)
  global.imageActorZ.setVisibility(x.data.showSliceZ)
  global.imageMapperX.setXSlice(x.data.sliceX);
  global.imageMapperY.setYSlice(x.data.sliceY);
  global.imageMapperZ.setZSlice(x.data.sliceZ);
  global.imageActorX.modified();
  global.imageActorY.modified();
  global.imageActorZ.modified();

  renderWindow.render();
}
