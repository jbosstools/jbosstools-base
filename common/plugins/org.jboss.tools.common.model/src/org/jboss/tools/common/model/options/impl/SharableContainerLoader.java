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
package org.jboss.tools.common.model.options.impl;

import org.w3c.dom.*;
import org.jboss.tools.common.model.options.*;
//import com.elt.model.viewedit.impl.flow.*;
import org.jboss.tools.common.model.*;

public class SharableContainerLoader extends SharableLoaderImpl {

    public SharableContainerLoader() {}

    public void loadChild(Element element, SharableElement sharable, String scopename) {
        SharableContainerImpl container = (SharableContainerImpl)sharable;
        XModelObject o = container.getChildForScope(scopename);
        String en = element.getAttribute("ENTITY");
        if(!o.getModelEntity().getName().equals(en)) return;
////        if(o instanceof FlowClassTemplateImpl) {
////            new ClassLoader_Impl().loadClass(element, (Class_)o);
////        }
    }

    public void saveChildren(Element element, SharableElement sharable, String scopename) {
/*        SharableContainerImpl container = (SharableContainerImpl)sharable;
        XModelObject o = container.getChildForScope(scopename);
        if(o instanceof FlowClassTemplateImpl) {
            new ClassLoader_Impl().saveClass(element, (Class_)o);
        }*/
    }

}
