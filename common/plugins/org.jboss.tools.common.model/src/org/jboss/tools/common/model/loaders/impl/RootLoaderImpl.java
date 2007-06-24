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
package org.jboss.tools.common.model.loaders.impl;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.util.*;

public class RootLoaderImpl implements XObjectLoader {

    public RootLoaderImpl() {}

    public void load(XModelObject object) {
        XChild[] cs = object.getModelEntity().getChildren();
        for (int i = 0; i < cs.length; i++) {
          if(!cs[i].isRequired() || cs[i].getMaxCount() != 1) continue;
          try {
              object.addChild(object.getModel().createModelObject(cs[i].getName(), new java.util.Properties()));
          } catch (Exception e) {
        	  //ignore
          }
        }
        XModelObject[] children = object.getChildren();
        for (int i = 0; i < children.length; i++) {
            XObjectLoader rl = XModelObjectLoaderUtil.getObjectLoader(children[i]);
            if(rl != null) rl.load(children[i]);
        }
    }

    public boolean update(XModelObject object) {
        XModelObject[] children = object.getChildren();
        boolean b = true;
        for (int i = 0; i < children.length; i++) {
            XObjectLoader rl = XModelObjectLoaderUtil.getObjectLoader(children[i]);
            if(rl != null && !rl.update(children[i])) b = false;
        }
        return b;
    }

    public boolean save(XModelObject object) {
        XModelObject[] children = object.getChildren();
        boolean b = true;
        for (int i = 0; i < children.length; i++) {
            XObjectLoader rl = XModelObjectLoaderUtil.getObjectLoader(children[i]);
            if(rl != null && !rl.save(children[i])) b = false;
        }
        if(b) object.setModified(false);
        return b;
    }

}

