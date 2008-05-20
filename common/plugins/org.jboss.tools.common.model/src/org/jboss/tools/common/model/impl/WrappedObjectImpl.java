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
package org.jboss.tools.common.model.impl;

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.*;

public class WrappedObjectImpl extends RegularObjectImpl {
	private static final long serialVersionUID = 1L;
	private XModelObject wrapped = null;

    public WrappedObjectImpl() {}

    public void setWrappedObject(Object wrapped) {
        if(wrapped instanceof XModelObject) this.wrapped = (XModelObject)wrapped;
    }

    public XModelObject getWrappedObject() {
        return wrapped;
    }

    public Image getImage() {
        return (wrapped == null) ? null : wrapped.getImage();
    }

    public Image getImage(String[] iconTypes) {
        return (wrapped == null) ? null : wrapped.getImage(iconTypes);
    }

    public String getAttributeValue(String attributeName) {
        return (wrapped == null ||
                wrapped.getModelEntity().getAttribute(attributeName) == null)
          ? super.getAttributeValue(attributeName)
          : wrapped.getAttributeValue(attributeName);
    }

    public String setAttributeValue(String attributeName, String value) {
        return (wrapped == null ||
                wrapped.getModelEntity().getAttribute(attributeName) == null)
          ? super.setAttributeValue(attributeName, value)
          : wrapped.setAttributeValue(attributeName, value);
    }

    public String getPathPart() {
        return (wrapped == null) ? super.getPathPart() : wrapped.getPathPart();
    }

    public String getPresentationString() {
        return (wrapped == null) ? super.getPresentationString()
               : wrapped.getPresentationString();
    }

}
