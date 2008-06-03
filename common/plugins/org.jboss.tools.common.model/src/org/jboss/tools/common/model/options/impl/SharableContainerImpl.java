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

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;

import java.util.*;
public class SharableContainerImpl extends SharableElementImpl {
    private static final long serialVersionUID = 7648767438226121456L;

    public SharableContainerImpl() {
        super();
    }

    protected void onSetEntity(String entityName) {
        XModelEntity entity = getModel().getMetaData().getEntity(entityName);
        XChild[] cs = entity.getChildren();
        if(cs.length > 0) {
            for (int i = 0; i < LIST.length; i++) {
                XScope s = (XScope)scopes.get(LIST[i]);
                XModelObjectImpl o = (XModelObjectImpl)getModel().createModelObject(cs[0].getName(), new Properties());
                o.setParent_0(this);
                s.setChild(o);
            }
        }
        super.onSetEntity(entityName);
    }

    public XModelObject[] getChildren() {
        XModelObject c = (XModelObject)scope.getChild();
        return (c == null) ? new XModelObject[0]
                           : new XModelObject[]{c};
    }

    public XModelObject getChildForScope(String scopename) {
        XScope scope = (XScope)scopes.get(scopename);
        if(scope == null) scope = project_;
        return scope.getChild();
    }

    public String setAttributeValue(String attributeName, String value) {
        String ov = super.getAttributeValue(attributeName);
        if(ov != null && ov.equals(value)) return value;
        String s = super.setAttributeValue(attributeName, value);
        if("scope".equals(attributeName) && !ov.equals(s)) {
            ((XModelImpl)getModel()).fireStructureChanged(this);
        }
        return s;
    }

    private void shareChild0(XScope fs, XScope ts) {
        XModelObject fc = fs.getChild();
        if(fc == null) return;
        XModelObjectImpl tc =  (XModelObjectImpl)fc.copy();
        tc.setParent_0(this);
        ts.setChild(tc);
    }

    protected void childsharing(String fromscope, String toscope) {
        shareChild0(getXScope(fromscope), getXScope(toscope));
        ((XModelImpl)getModel()).fireStructureChanged(this);
    }

    protected void copy_children(XModelObject copy, boolean transform) {
        SharableContainerImpl _copy = (SharableContainerImpl)copy;
        for (int i = 0; i < LIST.length; i++) {
            XScope sc = (XScope)scopes.get(LIST[i]);
            XScope _sc = (XScope)_copy.scopes.get(LIST[i]);
            XModelObjectImpl ccur = (XModelObjectImpl)sc.getChild();
            XModelObjectImpl ccop = (XModelObjectImpl)ccur.copy(transform);
            ccop.setParent_0(_copy);
            _sc.setChild(ccop);
        }
    }

    protected void fireUpdateChilds() {
        XModelObject c = getChildAt(0);
        if(c != null) c.fireObjectChanged(null);
    }

    protected void mergeChildren(String fromscope, String toscope, boolean existed, boolean merge_all) {
        if(!existed) shareChild0(getXScope(fromscope), getXScope(toscope));
    }

}

