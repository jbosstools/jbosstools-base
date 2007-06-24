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

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.ui.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class ContainerChooserAdapter extends DefaultValueAdapter implements IActionHelper {	
	
	public String invoke(Control control) {
		return invoke0(control);			
	}
	
	public String getCommand() {
		return "Browse...";
	}
	
	public String invoke0(Control control) {
		String v = getStringValue(true);
		IPath p = v.length() == 0 ? null : new Path(v);
		IResource r = (p == null) ? null : ModelPlugin.getWorkspace().getRoot().findMember(p);
		IContainer c = (r instanceof IContainer) ? (IContainer)r : null;
		ContainerSelectionDialog dialog = new ContainerSelectionDialog(control.getShell(), c, false, null);
		dialog.create();
		if(dialog.open() != 0) return null;
		Object[] os = dialog.getResult();
		return os == null || os.length == 0 ? null : os[0].toString();
	}
	
	public Object getAdapter(Class adapter) {
		if (adapter == IActionHelper.class) return this;
		return super.getAdapter(adapter);
	}
	
}