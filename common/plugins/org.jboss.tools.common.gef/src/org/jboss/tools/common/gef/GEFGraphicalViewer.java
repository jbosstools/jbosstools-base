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
package org.jboss.tools.common.gef;

import org.eclipse.gef.ui.parts.ScrollingGraphicalViewer;

public class GEFGraphicalViewer extends ScrollingGraphicalViewer {
	private boolean flag = false;
	private GEFEditor editor;

	public GEFGraphicalViewer(GEFEditor editor) {
		super();
		this.editor = editor;
	}
	
	public void setNoDeselect(){
		flag = true;
	}
	
	public GEFEditor getGEFEditor(){
		return editor;
	}


	public void deselectAll() {
		if(!flag)super.deselectAll();
		else flag = false;
	}

}
