/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.meta.action;

import java.util.Hashtable;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.meta.action.impl.*;

public class SignificanceMessageFactory {
  
  static public final String MESSAGE_CLASS_NAME_PARAMETER = "significanceMessageClass";

  static private SignificanceMessage defaultFactory = new SignificanceMessageImpl();
  static private Hashtable map = new Hashtable();
  
  private SignificanceMessageFactory() {
    // Nobody can create message factory
  }
  
  public static SignificanceMessageFactory getInstance() {
      return SignificanceMessageFactoryHolder.instance;   
  }
  
  public String getMessage(XAction action, XModelObject object, XModelObject[] objects) {
    String ms = action.getProperty(MESSAGE_CLASS_NAME_PARAMETER);
    if(ms == null || ms.length() == 0) return defaultFactory.getMessage(action, object, objects);
    SignificanceMessage specificMessage = (SignificanceMessage)map.get(ms);
    if(specificMessage==null) {
      specificMessage = getImplInstance(ms);
    }
    return specificMessage.getMessage(action, object, objects);
  }

  private SignificanceMessage getImplInstance(String clsname) {
      try {
          return (SignificanceMessage)ModelFeatureFactory.getInstance().createFeatureInstance(clsname);
      } catch (Exception t) {
    	  //ignore
          return defaultFactory;
      }
  }
  
  static class SignificanceMessageFactoryHolder {
    static SignificanceMessageFactory instance = new SignificanceMessageFactory();
  }
}

