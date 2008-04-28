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

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IPathEditorInput;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.jboss.tools.common.model.XModelObject;

public class ModelObjectLocationEditorInput extends ModelObjectEditorInput implements ILocationProvider, IPathEditorInput {
	IPath path;

	public ModelObjectLocationEditorInput(XModelObject object, IPath path) {
		super(object);
		this.path = path;
	}

	public IPath getPath(Object element) {
		return path;
	}

	protected boolean isEditable() {
		if(path != null) {
			java.io.File f = path.toFile();
			return f == null || !f.isFile() || f.canWrite();
		}
		return true;
	}

	public Object getAdapter(Class adapter)	{
		if(adapter == ILocationProvider.class) {
			return this;
		}
		return super.getAdapter(adapter);
	}

	public boolean equals(Object o)	{
		if(super.equals(o)) return true;
		if(o instanceof IPathEditorInput) {
			IPath p = ((IPathEditorInput)o).getPath();
			return path != null && path.equals(p);
		}
		return false;
	}

	public IPath getPath() {
		return path;
	}

}

