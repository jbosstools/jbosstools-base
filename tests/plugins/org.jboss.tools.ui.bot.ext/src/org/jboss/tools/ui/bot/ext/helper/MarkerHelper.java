/*******************************************************************************
 * Copyright (c) 2007-2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.helper;

import static org.junit.Assert.assertFalse;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * Helper for Marker related tests
 * @author Vladimir Pakan
 *
 */
public class MarkerHelper {
  private IResource resource;
  
  public MarkerHelper (String resourceName, String... pathToResource){
    this.resource = ResourcesPlugin
      .getWorkspace()
      .getRoot()
      .findMember(MarkerHelper.getPathToResource(resourceName, pathToResource));
  }
  /**
   * Returns Markers for specified resource recursively
   * @param resourceName
   * @param pathToResource
   * @return
   */
  public static IMarker[] getResourceMarkers(String resourceName, String... pathToResource){
    return MarkerHelper.getResourceMarkers(null, resourceName, pathToResource);
  }
  /**
   * Returns Markers of specified type for specified resource recursively
   * @param type
   * @param resourceName
   * @param pathToResource
   * @return
   */
  public static IMarker[] getResourceMarkers(String type , String resourceName, String... pathToResource){
    try {
      return ResourcesPlugin
          .getWorkspace()
          .getRoot()
          .findMember(MarkerHelper.getPathToResource(resourceName, pathToResource))
          .findMarkers(type, true, IResource.DEPTH_INFINITE);
    } catch (CoreException ce) {
      throw new RuntimeException(ce);
    }
  }
  /**
   * Returns Markers of specified type for resource specified within constructor recursively
   * @param type
   * @return
   */
  public IMarker[] getMarkers(String type){
    try {
      return resource.findMarkers(type, true, IResource.DEPTH_INFINITE);
    } catch (CoreException ce) {
      throw new RuntimeException(ce);
    }    
  }
  /**
   * Returns Markers for resource specified within constructor recursively
   * @return
   */
  public IMarker[] getMarkers(){
    return getMarkers(null); 
  }
  /**
   * Returns path to specified resource
   * @param resourceName
   * @param pathToResource
   * @return
   */
  private static IPath getPathToResource (String resourceName, String... pathToResource){
    IPath path = new Path("/");
    
    if (pathToResource != null){
      for (String pathElement : pathToResource){
        path = path.append(pathElement);  
      }
    }  
    return path.append(resourceName);
    
  }
  /**
   * Returns Marker Line Number
   * @param marker
   * @return
   */
  public static String getMarkerLineNumber (IMarker marker){
    try {
      return marker.getAttribute(IMarker.LINE_NUMBER).toString();
    } catch (CoreException ce) {
      return "";
    }
  }
  /**
   * Returns Marker Message
   * @param marker
   * @return
   */
  public static String getMarkerMessage (IMarker marker){
    return marker.getAttribute(IMarker.MESSAGE, "");
  }
  /**
   * Returns Marker Resource Name
   * @param marker
   * @return
   */
  public static String getMarkerResourceName (IMarker marker){
    return marker.getResource().getName();
  }
  /**
   * Returns Marker Resource Location i.e. path to resource without resource name 
   * @param marker
   * @return
   */
  public static String getMarkerResourceLocation (IMarker marker){
    return marker.getResource().getParent().getFullPath().toString();
  }
  /**
   * Display all markers of type for resource specified within constructor
   * @param type
   */
  public void displayAllMarkers(String type){
    displayMarkers(getMarkers(type));
  }
  /**
   * Display all markers for resource specified within constructor
   */
  public void displayAllMarkers(){
    displayAllMarkers(null);
  }
  /**
   * Display all Markers of specified type for specified resource recursively
   * @param resourceName
   * @param pathToResource
   * @param type
   */
  public static void displayAllMarkers(String type, String resourceName, String... pathToResource){
    MarkerHelper.displayMarkers(
      MarkerHelper.getResourceMarkers(type , resourceName, pathToResource));
  }
  /**
   * Display all Markers for specified resource recursively
   * @param resourceName
   * @param pathToResource
   */
  public static void displayAllMarkers(String resourceName, String... pathToResource){
    MarkerHelper.displayAllMarkers(null, resourceName, pathToResource);
  }
  /**
   * Display markers nicely formatted
   */
  private static void displayMarkers (IMarker[] markers) {
    for (IMarker marker : markers){
      StringBuffer sb = new StringBuffer("");
      sb.append("Line Number: ");
      sb.append(MarkerHelper.getMarkerLineNumber(marker));
      sb.append(" Location: ");
      sb.append(MarkerHelper.getMarkerResourceLocation(marker));
      sb.append(" Resource Name: ");
      sb.append(MarkerHelper.getMarkerResourceName(marker));
      sb.append(" Message: ");
      sb.append(MarkerHelper.getMarkerMessage(marker));
      System.out.println(sb);
      try {
        sb = new StringBuffer(" - attributes:");
        for (Object key : marker.getAttributes().keySet()){
          sb.append(" ");
          sb.append(key);
          sb.append("=");
          sb.append(marker.getAttribute(key.toString()));
        }
        System.out.println(sb);
      } catch (CoreException ce) {
        throw new RuntimeException(ce);
      }
    }
  }
  /**
   * Check if resource specified within constructor has marker
   * on specified line of specified type and with message matching decritpionRegex
   * @param lineNumber
   * @param descriptionRegex
   * @param type
   */
  public void checkForMarker (String lineNumber , String descriptionRegex , String type){
    MarkerHelper.checkMarkersForMarker(getMarkers(type), 
        lineNumber,
        descriptionRegex,
        resource.getName());
  }
  /**
  * Check if resource specified within constructor has marker
  * on specified line and with message matching decritpionRegex
  * @param lineNumber
  * @param descriptionRegex
  */
  public void checkForMarker (String lineNumber , String descriptionRegex ){
    checkForMarker(lineNumber, descriptionRegex, null);
  }
  /**
   * Check if specified resource has marker
   * on specified line and with message matching decritpionRegex
   * @param lineNumber
   * @param descriptionRegex
   * @param resourceName
   * @param pathToResource
   */
  public static void checkResourceForMarker (String lineNumber , String descriptionRegex, String resourceName, String... pathToResource){
    MarkerHelper.checkResourceForMarker(null, 
        lineNumber, 
        descriptionRegex, 
        resourceName,
        pathToResource);
  }
  /**
   * Check if specified resource has marker
   * on specified line of specified type and with message matching decritpionRegex
   * @param type
   * @param lineNumber
   * @param descriptionRegex
   * @param resourceName
   * @param pathToResource
   */
  public static void checkResourceForMarker (String type , String lineNumber , String descriptionRegex, String resourceName, String... pathToResource){
    MarkerHelper.checkMarkersForMarker(MarkerHelper.getResourceMarkers(type , resourceName, pathToResource), 
        lineNumber,
        descriptionRegex,
        resourceName);
  }
  /**
   * Check if markers contains marker
   * on specified line and with message matching decritpionRegex
   * @param markers
   * @param lineNumber
   * @param descriptionRegex
   * @param resourceName
   */
  private static void checkMarkersForMarker (IMarker[] markers , String lineNumber , String descriptionRegex , String resourceName){
    boolean notFound = true;
    int index = 0;
    while (notFound && index < markers.length){
      if (MarkerHelper.getMarkerLineNumber(markers[index]).equals(lineNumber)){
        if (MarkerHelper.getMarkerMessage(markers[index]).matches(descriptionRegex)){
          notFound = false;
        }
      }
      index++;
    }
    assertFalse("Resource: " + resourceName +
        "doesn't have marker on line " + lineNumber +
        " with descritpion matching regular expression '" + descriptionRegex + "'",
      notFound);
  }
  
}
