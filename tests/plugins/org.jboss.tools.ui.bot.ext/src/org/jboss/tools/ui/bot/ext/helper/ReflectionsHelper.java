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
  
} 
