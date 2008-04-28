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
package org.jboss.tools.common.model.ui.texteditors.dnd;

import java.util.Properties;

import org.eclipse.jface.text.source.ISourceViewer;
import org.jboss.tools.common.model.XModelObject;

public interface TextEditorDropProvider {
	public ISourceViewer getSourceViewer();
	public XModelObject getModelObject();
	public void insert(Properties p);	
}
