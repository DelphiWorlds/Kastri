package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2021 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.content.Context;
import android.content.SharedPreferences;
import com.google.android.gms.maps.model.LatLng;

// gson.jar (2.8.6)
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.HashMap;

public class GeofenceRegions {

    public class Region {
        private String mId;
        private LatLng mCoords;
        private double mRadius;
        private int mTransitionTypes;

        public Region(String id, LatLng coords, double radius, int transitionTypes) {
            mId = id;
            mCoords = coords;
            mRadius = radius;
            mTransitionTypes = transitionTypes;
        }

        public String getId() {
            return mId;
        }

        public LatLng getCoords() {
            return mCoords;
        }

        public double getRadius() {
            return mRadius;
        }

        public int getTransitionTypes() {
            return mTransitionTypes;
        }
    }

    private static GeofenceRegions instance = null;
    private Context mContext;
    private HashMap<String, Region> mItems = new HashMap<String, Region>();
    private Type mType = new TypeToken<HashMap<String, Region>>(){}.getType();  

    private void loadItems() {
        SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
        if (pref != null) {       
            try {
                Gson gson = new Gson();
                String json = pref.getString("Regions", "");
                if (json != null && !json.isEmpty())
                    mItems = (HashMap<String, Region>) gson.fromJson(json, mType);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void saveItems() {
        if ((mItems == null) || mItems.isEmpty())
            return;
        SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
        if (pref != null){
            SharedPreferences.Editor editor = pref.edit();
            editor.putString("Regions", toJson());
            editor.commit();
        } // else ????
    }

    public static GeofenceRegions getInstance(Context context) {
        if (instance == null)
            instance = new GeofenceRegions(context);
        return instance;
    }

    public GeofenceRegions(Context context) {
        mContext = context;
    }

    public HashMap<String, Region> getItems() {
        return mItems;
    }

    public void add(String id, double latitude, double longitude, double radius, int transitionTypes) {
        mItems.put(id, new Region(id, new LatLng(latitude, longitude), radius, transitionTypes));
        saveItems();
    }

    public void clear() {
        mItems.clear();
        saveItems();
    }

    public Region get(String id) {
        return mItems.get(id);
    }

    public void load() {
        loadItems();
    }

    public void remove(String id) {
        mItems.remove(id);
        saveItems();
    }

    public String toJson() {
        return new Gson().toJson(mItems, mType);     
    }

}