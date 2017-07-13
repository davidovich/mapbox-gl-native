package com.mapbox.mapboxsdk.maps;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verify;

public class MapChangeDispatcherTest {

  private MapChangeDispatcher mapChangeDispatcher;

  @Mock
  private MapView.OnCameraWillChangeListener onCameraWillChangeListener;

  @Mock
  private MapView.OnCameraDidChangeListener onCameraDidChangeListener;

  @Mock
  private MapView.OnCameraIsChangingListener onCameraIsChangingListener;

  @Mock
  private MapView.OnWillStartLoadingMapListener onWillStartLoadingMapListener;

  @Mock
  private MapView.OnDidFinishLoadingMapListener onDidFinishLoadingMapListener;

  @Mock
  private MapView.OnDidFailLoadingMapListener onDidFailLoadingMapListener;

  @Mock
  private MapView.OnWillStartRenderingFrameListener onWillStartRenderingFrameListener;

  @Mock
  private MapView.OnDidFinishRenderingFrameListener onDidFinishRenderingFrameListener;

  @Mock
  private MapView.OnWillStartRenderingMapListener onWillStartRenderingMapListener;

  @Mock
  private MapView.OnDidFinishRenderingMapListener onDidFinishRenderingMapListener;

  @Mock
  private MapView.OnDidFinishLoadingStyleListener onDidFinishLoadingStyleListener;

  @Mock
  private MapView.OnSourceChangedListener onSourceChangedListener;

  @Mock
  private MapView.OnMapChangedListener onMapChangedListener;

  @Mock
  private MapView.MapChangeResultHandler mapCallback;

  @Before
  public void beforeTest() {
    MockitoAnnotations.initMocks(this);
    mapChangeDispatcher = new MapChangeDispatcher();
    mapChangeDispatcher.addOnMapChangedListener(onMapChangedListener);
    mapChangeDispatcher.bind(mapCallback);
  }

  @Test
  public void testOnCameraRegionWillChangeListener() throws Exception {
    mapChangeDispatcher.setOnCameraWillChangeListener(onCameraWillChangeListener);
    mapChangeDispatcher.onCameraWillChange(false);
    verify(onCameraWillChangeListener).onCameraWillChange(false);
    verify(onMapChangedListener).onMapChanged(MapView.REGION_WILL_CHANGE);
  }

  @Test
  public void testOnCameraRegionWillChangeAnimatedListener() throws Exception {
    mapChangeDispatcher.setOnCameraWillChangeListener(onCameraWillChangeListener);
    mapChangeDispatcher.onCameraWillChange(true);
    verify(onCameraWillChangeListener).onCameraWillChange(true);
    verify(onMapChangedListener).onMapChanged(MapView.REGION_WILL_CHANGE_ANIMATED);
  }

  @Test
  public void testOnCameraIsChangingListener() throws Exception {
    mapChangeDispatcher.setOnCameraIsChangingListener(onCameraIsChangingListener);
    mapChangeDispatcher.onCameraIsChanging();
    verify(onCameraIsChangingListener).onCameraIsChanging();
    verify(onMapChangedListener).onMapChanged(MapView.REGION_IS_CHANGING);
    verify(mapCallback).onCameraIsChanging();
  }

  @Test
  public void testOnCameraRegionDidChangeListener() throws Exception {
    mapChangeDispatcher.setOnCameraDidChangeListener(onCameraDidChangeListener);
    mapChangeDispatcher.onCameraDidChange(false);
    verify(onCameraDidChangeListener).onCameraDidChange(false);
    verify(onMapChangedListener).onMapChanged(MapView.REGION_DID_CHANGE);
    verify(mapCallback).onCameraDidChange(false);
  }

  @Test
  public void testOnCameraRegionDidChangeAnimatedListener() throws Exception {
    mapChangeDispatcher.setOnCameraDidChangeListener(onCameraDidChangeListener);
    mapChangeDispatcher.onCameraDidChange(true);
    verify(onCameraDidChangeListener).onCameraDidChange(true);
    verify(onMapChangedListener).onMapChanged(MapView.REGION_DID_CHANGE_ANIMATED);
    verify(mapCallback).onCameraDidChange(true);
  }

  @Test
  public void testOnWillStartLoadingMapListener() throws Exception {
    mapChangeDispatcher.setOnWillStartLoadingMapListener(onWillStartLoadingMapListener);
    mapChangeDispatcher.onWillStartLoadingMap();
    verify(onWillStartLoadingMapListener).onWillStartLoadingMap();
    verify(onMapChangedListener).onMapChanged(MapView.WILL_START_LOADING_MAP);
  }

  @Test
  public void testOnDidFinishLoadingMapListener() throws Exception {
    mapChangeDispatcher.setOnDidFinishLoadingMapListener(onDidFinishLoadingMapListener);
    mapChangeDispatcher.onDidFinishLoadingMap();
    verify(onDidFinishLoadingMapListener).onDidFinishLoadingMap();
    verify(onMapChangedListener).onMapChanged(MapView.DID_FINISH_LOADING_MAP);
    verify(mapCallback).onDidFinishLoadingMap();
  }

  @Test
  public void testOnDidFailLoadingMapListener() throws Exception {
    String errorMessage =  "fail";
    mapChangeDispatcher.setOnDidFailLoadingMapListener(onDidFailLoadingMapListener);
    mapChangeDispatcher.onDidFailLoadingMap(errorMessage);
    verify(onDidFailLoadingMapListener).onDidFailLoadingMap(errorMessage);
    verify(onMapChangedListener).onMapChanged(MapView.DID_FAIL_LOADING_MAP);
  }

  @Test
  public void testOnWillStartRenderingFrameListener() throws Exception {
    mapChangeDispatcher.setOnWillStartRenderingFrameListener(onWillStartRenderingFrameListener);
    mapChangeDispatcher.onWillStartRenderingFrame();
    verify(onWillStartRenderingFrameListener).onWillStartRenderingFrame();
    verify(onMapChangedListener).onMapChanged(MapView.WILL_START_RENDERING_FRAME);
  }

  @Test
  public void testOnDidFinishRenderingFrameListener() throws Exception {
    mapChangeDispatcher.setOnDidFinishRenderingFrameListener(onDidFinishRenderingFrameListener);
    mapChangeDispatcher.onDidFinishRenderingFrame(true);
    verify(onDidFinishRenderingFrameListener).onDidFinishRenderingFrame(true);
    verify(onMapChangedListener).onMapChanged(MapView.DID_FINISH_RENDERING_FRAME);
    verify(mapCallback).onDidFinishRenderingFrame(true);
  }

  @Test
  public void testOnDidFinishRenderingFrameFullyRenderedListener() throws Exception {
    mapChangeDispatcher.setOnDidFinishRenderingFrameListener(onDidFinishRenderingFrameListener);
    mapChangeDispatcher.onDidFinishRenderingFrame(false);
    verify(onDidFinishRenderingFrameListener).onDidFinishRenderingFrame(false);
    verify(onMapChangedListener).onMapChanged(MapView.DID_FINISH_RENDERING_FRAME_FULLY_RENDERED);
    verify(mapCallback).onDidFinishRenderingFrame(false);
  }

  @Test
  public void testOnWillStartRenderingMapListener() throws Exception {
    mapChangeDispatcher.setOnWillStartRenderingMapListener(onWillStartRenderingMapListener);
    mapChangeDispatcher.onWillStartRenderingMap();
    verify(onWillStartRenderingMapListener).onWillStartRenderingMap();
    verify(onMapChangedListener).onMapChanged(MapView.WILL_START_RENDERING_MAP);
  }

  @Test
  public void testOnDidFinishRenderingMapListener() throws Exception {
    mapChangeDispatcher.setOnDidFinishRenderingMapListener(onDidFinishRenderingMapListener);
    mapChangeDispatcher.onDidFinishRenderingMap(true);
    verify(onDidFinishRenderingMapListener).onDidFinishRenderingMap(true);
    verify(onMapChangedListener).onMapChanged(MapView.DID_FINISH_RENDERING_MAP);
  }

  @Test
  public void testOnDidFinishRenderingMapFullyRenderedListener() throws Exception {
    mapChangeDispatcher.setOnDidFinishRenderingMapListener(onDidFinishRenderingMapListener);
    mapChangeDispatcher.onDidFinishRenderingMap(false);
    verify(onDidFinishRenderingMapListener).onDidFinishRenderingMap(false);
    verify(onMapChangedListener).onMapChanged(MapView.DID_FINISH_RENDERING_MAP_FULLY_RENDERED);
  }

  @Test
  public void testOnDidFinishLoadingStyleListener() throws Exception {
    mapChangeDispatcher.setOnDidFinishLoadingStyleListener(onDidFinishLoadingStyleListener);
    mapChangeDispatcher.onDidFinishLoadingStyle();
    verify(onDidFinishLoadingStyleListener).onDidFinishLoadingStyle();
    verify(onMapChangedListener).onMapChanged(MapView.DID_FINISH_LOADING_STYLE);
    verify(mapCallback).onDidFinishLoadingStyle();
  }

  @Test
  public void testOnSourceChangedListener() throws Exception {
    String sourceId = "sourceId";
    mapChangeDispatcher.setOnSourceChangedListener(onSourceChangedListener);
    mapChangeDispatcher.onSourceChanged(sourceId);
    verify(onSourceChangedListener).onSourceChangedListener(sourceId);
    verify(onMapChangedListener).onMapChanged(MapView.SOURCE_DID_CHANGE);
  }
}
