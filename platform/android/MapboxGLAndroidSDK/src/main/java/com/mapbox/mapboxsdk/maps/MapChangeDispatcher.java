package com.mapbox.mapboxsdk.maps;

import java.util.concurrent.CopyOnWriteArrayList;

import timber.log.Timber;

/*
 * Class responsible for forwarding map change events emitted by core.
 * <p>
 * Events are dispatched to end-user defined callbacks and MapView.MapChangeResultHandler.
 * </p>
 * <p>
 * This class is responsible for managing the deprecated MapView.OnMapChangeListener API.
 * </p>
 */
class MapChangeDispatcher {

  // Support deprecated API
  private final CopyOnWriteArrayList<MapView.OnMapChangedListener> onMapChangedListeners = new CopyOnWriteArrayList<>();

  // End-user callbacks
  private MapView.OnCameraWillChangeListener onCameraWillChangeListener;
  private MapView.OnCameraIsChangingListener onCameraIsChangingListener;
  private MapView.OnCameraDidChangeListener onCameraDidChangeListener;
  private MapView.OnWillStartLoadingMapListener onWillStartLoadingMapListener;
  private MapView.OnDidFinishLoadingMapListener onDidFinishLoadingMapListener;
  private MapView.OnDidFailLoadingMapListener onDidFailLoadingMapListener;
  private MapView.OnWillStartRenderingFrameListener onWillStartRenderingFrameListener;
  private MapView.OnDidFinishRenderingFrameListener onDidFinishRenderingFrameListener;
  private MapView.OnWillStartRenderingMapListener onWillStartRenderingMapListener;
  private MapView.OnDidFinishRenderingMapListener onDidFinishRenderingMapListener;
  private MapView.OnDidFinishLoadingStyleListener onDidFinishLoadingStyleListener;
  private MapView.OnSourceChangedListener onSourceChangedListener;

  // Internal components callback
  private MapView.MapChangeResultHandler mapChangeResultHandler;

  /*
   * Binds the result handler to notify internal components about map change events.
   */
  void bind(MapView.MapChangeResultHandler mapChangeResultHandler) {
    this.mapChangeResultHandler = mapChangeResultHandler;
  }

  void setOnCameraWillChangeListener(MapView.OnCameraWillChangeListener listener) {
    this.onCameraWillChangeListener = listener;
  }

  void setOnCameraIsChangingListener(MapView.OnCameraIsChangingListener listener) {
    this.onCameraIsChangingListener = listener;
  }

  void setOnCameraDidChangeListener(MapView.OnCameraDidChangeListener listener) {
    this.onCameraDidChangeListener = listener;
  }

  void setOnWillStartLoadingMapListener(MapView.OnWillStartLoadingMapListener listener) {
    this.onWillStartLoadingMapListener = listener;
  }

  void setOnDidFinishLoadingMapListener(MapView.OnDidFinishLoadingMapListener listener) {
    this.onDidFinishLoadingMapListener = listener;
  }

  void setOnDidFailLoadingMapListener(MapView.OnDidFailLoadingMapListener listener) {
    this.onDidFailLoadingMapListener = listener;
  }

  void setOnWillStartRenderingFrameListener(MapView.OnWillStartRenderingFrameListener listener) {
    this.onWillStartRenderingFrameListener = listener;
  }

  void setOnDidFinishRenderingFrameListener(MapView.OnDidFinishRenderingFrameListener listener) {
    this.onDidFinishRenderingFrameListener = listener;
  }

  void setOnWillStartRenderingMapListener(MapView.OnWillStartRenderingMapListener listener) {
    this.onWillStartRenderingMapListener = listener;
  }

  void setOnDidFinishRenderingMapListener(MapView.OnDidFinishRenderingMapListener listener) {
    this.onDidFinishRenderingMapListener = listener;
  }

  void setOnDidFinishLoadingStyleListener(MapView.OnDidFinishLoadingStyleListener listener) {
    onDidFinishLoadingStyleListener = listener;
  }

  void setOnSourceChangedListener(MapView.OnSourceChangedListener listener) {
    onSourceChangedListener = listener;
  }

  void onCameraWillChange(boolean animated) {
    if (onCameraWillChangeListener != null) {
      onCameraWillChangeListener.onCameraWillChange(animated);
    }
    onMapChange(animated ? MapView.REGION_WILL_CHANGE_ANIMATED : MapView.REGION_WILL_CHANGE);
  }

  void onCameraIsChanging() {
    if (onCameraIsChangingListener != null) {
      onCameraIsChangingListener.onCameraIsChanging();
    }
    if (mapChangeResultHandler != null) {
      mapChangeResultHandler.onCameraIsChanging();
    }
    onMapChange(MapView.REGION_IS_CHANGING);
  }

  void onCameraDidChange(boolean animated) {
    if (onCameraDidChangeListener != null) {
      onCameraDidChangeListener.onCameraDidChange(animated);
    }
    if (mapChangeResultHandler != null) {
      mapChangeResultHandler.onCameraDidChange(animated);
    }
    onMapChange(animated ? MapView.REGION_DID_CHANGE_ANIMATED : MapView.REGION_DID_CHANGE);
  }

  void onWillStartLoadingMap() {
    if (onWillStartLoadingMapListener != null) {
      onWillStartLoadingMapListener.onWillStartLoadingMap();
    }
    onMapChange(MapView.WILL_START_LOADING_MAP);
  }

  void onDidFinishLoadingMap() {
    if (onDidFinishLoadingMapListener != null) {
      onDidFinishLoadingMapListener.onDidFinishLoadingMap();
    }
    if (mapChangeResultHandler != null) {
      mapChangeResultHandler.onDidFinishLoadingMap();
    }
    onMapChange(MapView.DID_FINISH_LOADING_MAP);
  }

  void onDidFailLoadingMap(String errorMessage) {
    if (onDidFailLoadingMapListener != null) {
      onDidFailLoadingMapListener.onDidFailLoadingMap(errorMessage);
    }
    onMapChange(MapView.DID_FAIL_LOADING_MAP);
  }

  void onWillStartRenderingFrame() {
    if (onWillStartRenderingFrameListener != null) {
      onWillStartRenderingFrameListener.onWillStartRenderingFrame();
    }
    onMapChange(MapView.WILL_START_RENDERING_FRAME);
  }

  void onDidFinishRenderingFrame(boolean partial) {
    if (onDidFinishRenderingFrameListener != null) {
      onDidFinishRenderingFrameListener.onDidFinishRenderingFrame(partial);
    }
    if (mapChangeResultHandler != null) {
      mapChangeResultHandler.onDidFinishRenderingFrame(partial);
    }
    onMapChange(partial ? MapView.DID_FINISH_RENDERING_FRAME : MapView.DID_FINISH_RENDERING_FRAME_FULLY_RENDERED);
  }

  void onWillStartRenderingMap() {
    if (onWillStartRenderingMapListener != null) {
      onWillStartRenderingMapListener.onWillStartRenderingMap();
    }
    onMapChange(MapView.WILL_START_RENDERING_MAP);
  }

  void onDidFinishRenderingMap(boolean partial) {
    if (onDidFinishRenderingMapListener != null) {
      onDidFinishRenderingMapListener.onDidFinishRenderingMap(partial);
    }
    onMapChange(partial ? MapView.DID_FINISH_RENDERING_MAP : MapView.DID_FINISH_RENDERING_MAP_FULLY_RENDERED);
  }

  void onDidFinishLoadingStyle() {
    if (onDidFinishLoadingStyleListener != null) {
      onDidFinishLoadingStyleListener.onDidFinishLoadingStyle();
    }
    if (mapChangeResultHandler != null) {
      mapChangeResultHandler.onDidFinishLoadingStyle();
    }
    onMapChange(MapView.DID_FINISH_LOADING_STYLE);
  }

  void onSourceChanged(String id) {
    if (onSourceChangedListener != null) {
      onSourceChangedListener.onSourceChangedListener(id);
    }
    onMapChange(MapView.SOURCE_DID_CHANGE);
  }

  //
  // Deprecated API since 5.2.0
  //

  void onMapChange(int onMapChange) {
    if (!onMapChangedListeners.isEmpty()) {
      for (MapView.OnMapChangedListener onMapChangedListener : onMapChangedListeners) {
        try {
          onMapChangedListener.onMapChanged(onMapChange);
        } catch (RuntimeException err) {
          Timber.e("Exception (%s) in MapView.OnMapChangedListener: %s", err.getClass(), err.getMessage());
        }
      }
    }
  }

  void addOnMapChangedListener(MapView.OnMapChangedListener listener) {
    onMapChangedListeners.add(listener);
  }

  void removeOnMapChangedListener(MapView.OnMapChangedListener listener) {
    onMapChangedListeners.remove(listener);
  }
}
