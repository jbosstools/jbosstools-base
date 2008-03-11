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
package org.jboss.tools.common.model.ui.editor;

import org.jboss.tools.common.core.resources.XModelObjectEditorInputFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;
import org.jboss.tools.common.model.XModelObject;

public class ModelObjectEditorInput implements IModelObjectEditorInput, IPersistableElement {
	protected XModelObject object;
	
	public ModelObjectEditorInput(XModelObject object) {
		this.object = object;
	}

	public XModelObject getXModelObject() {
		return object;
	}

	public boolean exists() {
		return object != null;
	}

	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	public String getName() {
		return object.getPresentationString();
	}

	public IPersistableElement getPersistable() {
		return this;
	}

	public String getToolTipText() {
		return object.getPresentationString();
	}

	public Object getAdapter(Class adapter)	{
		if(adapter == XModelObject.class) return getXModelObject();
		return object.getAdapter(adapter);
	}

	public String getFactoryId() {
		return XModelObjectEditorInputFactory.getFactoryId();
	}

	public void saveState(IMemento memento) {
		XModelObjectEditorInputFactory.saveState(memento, this);
	}
	
	public boolean equals(Object o)	{
		getXModelObject();
		if (this == o) return true;
		if(o instanceof IModelObjectEditorInput) {
			return object.equals(((IModelObjectEditorInput)o).getXModelObject());
		}
		return false;
	}

}
