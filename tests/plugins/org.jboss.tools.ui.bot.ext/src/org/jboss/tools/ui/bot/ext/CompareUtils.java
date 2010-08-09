/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext;
/**
 * Utils for Css Tests
 * @author Vladimir Pakan
 */
public class CompareUtils {
  /**
   * Compare style attributes
   * @param attrs1
   * @param attrs2
   * @return
   */
  public static boolean compareStyleAttributes(String attrs1 , String attrs2){
    
    boolean result = false;
    
    if (attrs1 != null && attrs2 != null){
      String[] attrs1Split = attrs1.trim().split(";");
      String[] attrs2Split = attrs2.trim().split(";");
      if (attrs1Split.length == attrs2Split.length){
        if(attrs1Split.length > 0){
          boolean equals = true;
          for (int index = 0 ; equals && index < attrs1Split.length ; index++){
            equals = CompareUtils.compareInAttrsList(attrs1Split[index],attrs2Split);
            if (equals){
              equals = CompareUtils.compareInAttrsList(attrs2Split[index],attrs1Split);
            }
          }
          result = equals;
        }
        else{
          result = true;
        }
      }  
      else{
        result = false;
      }
    }
    else{
      result = (attrs1 == null) && (attrs2 == null);
    }
    
    return result;
    
  }
  /**
   * Compare style attribute
   * @param attr1
   * @param attr2
   * @return
   */
  private static boolean compareStyleAttribute (String attr1 , String attr2){
    
    boolean result = false;
    
    if (attr1 != null && attr2 != null){
      String[] attr1Split = attr1.trim().split(":");
      String[] attr2Split = attr2.trim().split(":");
      if (attr1Split.length == attr2Split.length){
        if(attr1Split.length > 0){
          boolean equals = true;
          for (int index = 0 ; equals && index < attr1Split.length ; index++){
            equals = attr1Split[index].trim().equals(attr2Split[index].trim());
          }
          result = equals;
        }
        else{
          result = true;
        }
      }
      else{
        result = false;
      }
    }
    else{
      result = (attr1 == null) && (attr2 == null);
    }
    
    return result;
    
  }
  /**
   * Search for attr in attrsList and compare values
   * @param attr
   * @param attrsList
   * @return
   */
  private static boolean compareInAttrsList (String attr , String[] attrsList){
   
    boolean result = false;
    
    for (int index = 0 ; !result && index < attrsList.length ; index++){
      result = CompareUtils.compareStyleAttribute(attr, attrsList[index]);
    }
    
    return result;
    
  }

}
