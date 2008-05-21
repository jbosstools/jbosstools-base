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
package org.jboss.tools.common.editor;

import org.eclipse.core.resources.IMarker;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.ui.IEditorInput;

import org.jboss.tools.common.model.XModelObject;

public interface ObjectTextEditor {
	public void setObject(XModelObject object);
	public void setCursor(int line, int position);
	public boolean isModified();
	public void setModified(boolean b);
	public void updateModification();
	public void addFocusListener(FocusListener listener);
	public void updateDocument();
	public void save();
	public void doSanityCheckState(IEditorInput input);
	public void gotoMarker(IMarker marker);
	public void selectModelObject(XModelObject object);
	public void selectModelObject(XModelObject object, String attribute);
	public XModelObject findModelObjectAtCursor();
	public void dispose();
}
