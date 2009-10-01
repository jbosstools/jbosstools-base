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
package org.jboss.tools.common.meta.action.impl;

import org.w3c.dom.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.impl.*;
import org.jboss.tools.common.model.XModelObjectConstants;

public class XAttributeDataImpl implements XAttributeData, XMetaDataConstants {
    private String entityname;
    private String attributename;
    private XAttribute attribute;
    private boolean mandatory;
    private boolean isAdvanced;
    private String value = null;
    private boolean references = false;
    private XEntityDataImpl entityData;

    public XAttributeDataImpl() {}

    public XEntityData getEntityData() {
    	return entityData;
    }

    public void setEntityData(XEntityDataImpl entityData) {
    	this.entityData = entityData;
    }

    public boolean getMandatoryFlag() {
        return mandatory && !isAdvanced;
    }
    public boolean isAdvanced() {
        return isAdvanced;
    }
    public String getValue() {
        return value;
    }
    public void setValue(String value) {
          this.value = value;
    }
    public XAttribute getAttribute() {
        return (references) ? attribute : (attribute = loadReference());
    }

    private XAttribute loadReference() {
        references = true;
        XModelMetaData meta = XModelMetaDataImpl.getInstance();
        if(meta == null) return null;
        XModelEntity ent = meta.getEntity(entityname);
        if(ent == null) return null;
        XAttribute[] attr = ent.getAttributes();
        for (int i = 0; i < attr.length; i++) {
            if(attr[i].getName().equals(attributename)) return attr[i];
        }
        return null;
    }

    public void setDefaultValue() {
        XAttribute a = getAttribute();
        if(a != null) setValue(a.getDefaultValue());
    }

    public void load(Element el) {
        attributename = el.getAttribute("AttributeName"); //$NON-NLS-1$
        entityname = ((Element)el.getParentNode()).getAttribute("EntityName"); //$NON-NLS-1$
        String m = el.getAttribute("Mandatory"); //$NON-NLS-1$
        mandatory = XModelObjectConstants.YES.equals(m) || "".equals(m); //$NON-NLS-1$
        m = el.getAttribute("Advanced"); //$NON-NLS-1$
        isAdvanced = XModelObjectConstants.YES.equals(m);
    }
    
    public String getAttributeName() {
    	return attributename;
    }

    public static XAttributeDataImpl create(String entityname, String attributename, boolean mandatory) {
        XAttributeDataImpl impl = new XAttributeDataImpl();
        impl.entityname = entityname;
        impl.attributename = attributename;
        impl.mandatory = mandatory;
        return impl;
    }

}

