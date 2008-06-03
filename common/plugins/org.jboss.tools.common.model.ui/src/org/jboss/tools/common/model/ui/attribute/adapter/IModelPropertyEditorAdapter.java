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
package org.jboss.tools.common.model.ui.attribute.adapter;

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;

public interface IModelPropertyEditorAdapter extends IValueProvider, IValueChangeListener, IAttributeErrorProvider {
	public void setModel(XModel model);
	public void setAttribute(XAttribute attribute);
	public XAttribute getAttribute();
	public void setModelObject(XModelObject modelObject);
	public void setAttributeData(XAttributeData attributeData);
	
	public void load(); // load from object (XModelObject or XAttributeData)
	public void store(); // store to object (XModelObject or XAttributeData)
	//	autoStote to object
	public void setAutoStore(boolean autoStore);
	// destroy object
	public void dispose();
}
