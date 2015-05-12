/*******************************************************************************
 * Copyright (c) 2007 - 2015 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext;

import org.eclipse.wst.sse.ui.StructuredTextEditor;

public interface IMultiPageEditor {
	/**
	 * Returns text editor which usually in tab "Source" of multipage editor
	 * @return
	 */
	public StructuredTextEditor getSourceEditor();
	
	/**
	 * Makes tab "Source" of multipage editor active
	 */
	public void switchToSourceTab();
}
