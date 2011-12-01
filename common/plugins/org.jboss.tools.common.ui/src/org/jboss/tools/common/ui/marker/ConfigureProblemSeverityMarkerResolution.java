/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.marker;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.internal.ui.DebugUIPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;

/**
 * @author Daniel Azarov
 */
public class ConfigureProblemSeverityMarkerResolution implements
		IMarkerResolution2 {
	private String preferencePageId;
	private String preferenceKey;
	
	public ConfigureProblemSeverityMarkerResolution(String preferencePageId, String preferenceKey){
		this.preferencePageId = preferencePageId;
		this.preferenceKey = preferenceKey;
	}

	public String getLabel() {
		return CommonUIMessages.CONFIGURE_PROBLEM_SEVERITY;
	}

	public void run(IMarker marker) {
		UIJob job = new UIJob(""){ //$NON-NLS-1$
			public IStatus runInUIThread(IProgressMonitor monitor) {
				PreferencesUtil.createPreferenceDialogOn(DebugUIPlugin.getShell(),
						ConfigureProblemSeverityMarkerResolution.this.preferencePageId,
						new String[]{ConfigureProblemSeverityMarkerResolution.this.preferencePageId},
						ConfigureProblemSeverityMarkerResolution.this.preferenceKey).open();
				return Status.OK_STATUS;
			}
		};
		job.setSystem(true);
		job.setPriority(Job.INTERACTIVE);
		job.schedule();
	}

	public String getDescription() {
		return CommonUIMessages.CONFIGURE_PROBLEM_SEVERITY;
	}

	public Image getImage() {
		String key = "DESC_ELCL_CONFIGURE_PROBLEM_SEVERITIES";
		ImageRegistry registry = CommonUIPlugin.getDefault().getImageRegistry();
		Image image = registry.get(key);
		if(image == null) {
			image = JavaPluginImages.DESC_ELCL_CONFIGURE_PROBLEM_SEVERITIES.createImage();
			registry.put(key, image);
		}		
		return image;
	}

}
