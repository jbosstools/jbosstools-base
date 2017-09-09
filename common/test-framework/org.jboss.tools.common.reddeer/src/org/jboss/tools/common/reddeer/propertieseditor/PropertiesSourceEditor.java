/*******************************************************************************
 * Copyright (c) 2015-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.propertieseditor;

import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.reddeer.workbench.impl.editor.TextEditor;
/**
 * RedDeer implementation of JBoss Properties file source editor
 * @author vlado pakan
 *
 */
public class PropertiesSourceEditor extends TextEditor{
	public PropertiesSourceEditor (ITextEditor textEditor){
		super(textEditor);
	}
}