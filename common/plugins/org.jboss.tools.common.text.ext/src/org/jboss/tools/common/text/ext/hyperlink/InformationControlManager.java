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
package org.jboss.tools.common.text.ext.hyperlink;

import org.eclipse.jface.text.AbstractInformationControlManager;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.text.ext.hyperlink.xpl.HierarchyInformationControl;
import org.jboss.tools.common.text.ext.hyperlink.xpl.InformationPresenter;

public class InformationControlManager {
	public static final InformationControlManager instance = new InformationControlManager();

	public IInformationControl showHyperlinks(String title, ITextViewer viewer, IHyperlink[] hyperlinks){
		return showHyperlinks(title, viewer, hyperlinks, false);
	}
	
	public IInformationControl showHyperlinks(String title, ITextViewer viewer, IHyperlink[] hyperlinks, boolean test){
		InformationPresenter presenter= new InformationPresenter(viewer, getHierarchyPresenterControlCreator(title, hyperlinks));
		presenter.setAnchor(AbstractInformationControlManager.ANCHOR_GLOBAL);
		presenter.setSizeConstraints(60, 10, true, false);
		presenter.install(viewer.getTextWidget());
		if(test){
			return presenter.showInformationForTest();
		} else {
			presenter.showInformation();
			return null;
		}
	}
	
	protected IInformationControlCreator getHierarchyPresenterControlCreator(final String title, final IHyperlink[] hyperlinks) {
		return new IInformationControlCreator() {
			public IInformationControl createInformationControl(Shell parent) {
				int shellStyle= SWT.RESIZE;
				int treeStyle= SWT.V_SCROLL | SWT.H_SCROLL;
				HierarchyInformationControl iControl = new HierarchyInformationControl(parent, title, shellStyle, treeStyle, hyperlinks);
				iControl.setInput(hyperlinks);
				return iControl;
			}
		};
	}
}
