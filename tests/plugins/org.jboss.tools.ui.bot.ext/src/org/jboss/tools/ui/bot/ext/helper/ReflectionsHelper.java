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

package org.jboss.tools.ui.bot.ext.helper;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.log4j.Logger;
/**
 * Helper to use Reflections functionality
 * @author Vladimir Pakan
 *
 */
public class ReflectionsHelper {
  protected static final Logger log = Logger.getLogger(ReflectionsHelper.class);
  
  @SuppressWarnings("unchecked")
  public static <T> T getPrivateFieldValue (Class<?> clazz , String fieldName , 
      Object instance, Class<T> resultClazz) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException{
    Field field = clazz.getDeclaredField(fieldName);
    field.setAccessible(true);
    Object value = field.get(instance);
    if (value != null){
      return (T)value;  
    }
    else{
      return null;
    }
  }
  /**
   * Invoke method methodName of class clazz on instance and return return value
   * @param <T>
   * @param clazz
   * @param methodName
   * @param instance
   * @param resultClazz
   * @return
   * @throws SecurityException
   * @throws NoSuchMethodException
   * @throws IllegalArgumentException
   * @throws IllegalAccessException
   * @throws InvocationTargetException
   */
  @SuppressWarnings("unchecked")
  public static <T> T retrieveMethodReturnValue(Class<?> clazz,
      String methodName, Object instance, Class<T> resultClazz)
      throws SecurityException, NoSuchMethodException,
      IllegalArgumentException, IllegalAccessException,
      InvocationTargetException {

    Method[] methods = clazz.getDeclaredMethods();
    boolean notFound = true;
    int index = 0;
    while (notFound && methods.length > index) {
      if (methods[index].getName().equals(methodName)) {
        notFound = false;
      } else {
        index++;
      }
    }

    if (!notFound) {
      methods[index].setAccessible(true);
      Object value = methods[index].invoke(instance);
      if (value != null) {
        return (T) value;
      } else {
        return null;
      }

    } else {
      throw new NoSuchMethodException(methodName);
    }
  }
  /**
   * Returns true if class clazz implements method methodName
   * @param clazz
   * @param methodName
   * @return
   */
  public static boolean isClassImplementingMethod(Class<?> clazz,
      String methodName) {

    Method[] methods = clazz.getDeclaredMethods();
    boolean notFound = true;
    int index = 0;
    while (notFound && methods.length > index) {
      if (methods[index].getName().equals(methodName)) {
        notFound = false;
      } else {
        index++;
      }
    }

    return !notFound;

  } 
  
} 
