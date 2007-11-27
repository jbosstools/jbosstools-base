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
package org.jboss.tools.common.verification.ui.vrules.wizard;

import org.eclipse.jface.internal.text.html.HTMLTextPresenter;
import org.eclipse.jface.text.*;
import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;

public class DescriptionManager extends AbstractHoverInformationControlManager {
//	private boolean fWindowsFlag;

	public DescriptionManager() {
		super(new DescriptionControlCreator());
	}

	protected void computeInformation() {
		Tree tree = (Tree)getSubjectControl();
		TreeItem item = tree.getItem(getHoverEventLocation());
		if (item != null) {
			TipSource source = (TipSource)item.getData();
			String description = source.getTip();
			setInformation(description, item.getBounds());
		} else {
			setInformation(null, null);
		}
	}

	public void setEnabled(boolean enabled) {
//		String osName = System.getProperty("os.name");
		boolean windowsFlag = true;
		//osName != null && osName.toUpperCase().indexOf("WINDOWS") != -1;
		super.setEnabled(enabled && windowsFlag);
	}
	
	static private class DescriptionControlCreator implements IInformationControlCreator {
		public IInformationControl createInformationControl(Shell parent) {
			return new DefaultInformationControl(parent, SWT.NONE, new HTMLTextPresenter(true));
		}
	}
}
