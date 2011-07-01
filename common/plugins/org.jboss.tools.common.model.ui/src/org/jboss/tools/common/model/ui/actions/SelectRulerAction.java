/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.model.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.source.IVerticalRulerInfo;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.ui.texteditor.AbstractRulerActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.common.model.ui.actions.xpl.SelectAnnotationRulerAction;

public class SelectRulerAction extends AbstractRulerActionDelegate {
	private IAction action=null;

	@Override
	protected IAction createAction(ITextEditor editor, IVerticalRulerInfo rulerInfo) {
		if(action == null)
		action = new SelectAnnotationRulerAction(ResourceBundle.getBundle("org.eclipse.jdt.internal.ui.javaeditor.ConstructedJavaEditorMessages"), "JavaSelectAnnotationRulerAction.", editor, rulerInfo); //$NON-NLS-1$
		return action;
	}
	
	public void mouseUp(MouseEvent e) {
		super.mouseUp(e);
		if(action != null && action.isEnabled())
			action.run();
	}
}
