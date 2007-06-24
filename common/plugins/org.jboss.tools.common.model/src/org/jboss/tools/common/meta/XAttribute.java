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
package org.jboss.tools.common.meta;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.meta.constraint.*;

public interface XAttribute extends XMetaElement {
	public XModelEntity getModelEntity();
    public XAttributeEditor getEditor();
    public String getDefaultValue();
    public boolean isVisible();
    public XAttributeConstraint getConstraint();
    public boolean isRequired();
    public boolean isEditable();
    public boolean isFake();
    public boolean isTrimmable();
    public boolean isCopyable();
    public String getXMLName();
    public void valueChanged(XModelObject object);
    public XAdapter getAdapter();
    public String getProperty(String name);
}
