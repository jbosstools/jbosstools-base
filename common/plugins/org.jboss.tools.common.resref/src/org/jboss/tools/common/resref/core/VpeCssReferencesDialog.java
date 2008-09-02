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
package org.jboss.tools.common.resref.core;

import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizard;

public class VpeCssReferencesDialog extends AbstractQueryWizard {
	
	public static boolean run(IFile file) {
		VpeCssReferencesDialog dialog = new VpeCssReferencesDialog();
		Properties p = new Properties();
		p.setProperty("help", "VpeCssReferencesDialog");
		p.put("file", file);
		p.put("model", PreferenceModelUtilities.getPreferenceModel());
		dialog.setObject(p);
		int code = dialog.execute();
		return code == 0;
	}

	public static boolean run(IPath path) {
		VpeCssReferencesDialog dialog = new VpeCssReferencesDialog();
		Properties p = new Properties();
		p.setProperty("help", "VpeCssReferencesDialog");
		p.put("path", path);
		p.put("model", PreferenceModelUtilities.getPreferenceModel());
		dialog.setObject(p);
		int code = dialog.execute();
		return code == 0;
	}

	public VpeCssReferencesDialog() {
		setView(new VpeCssReferencesDialogView());
	}

}

class VpeCssReferencesDialogView extends ResourceReferencesDialogView {
	
	public VpeCssReferencesDialogView() {}

	protected String getEntity() {
		return (file != null) ? "VPECSSReference" : "VPECSSReferenceExt";
	}

	protected ResourceReferencesTableProvider createTableProvider(List dataList) {
		return ResourceReferencesTableProvider.getCSSTableProvider(dataList);
	}

	protected ResourceReferenceList getReferenceList() {
		return CSSReferenceList.getInstance();
	}
	
}
