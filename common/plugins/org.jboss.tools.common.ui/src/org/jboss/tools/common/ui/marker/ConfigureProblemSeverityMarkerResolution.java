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

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.internal.ui.DebugUIPlugin;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.internal.dialogs.WorkbenchPreferenceNode;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.preferences.SeverityPreferencePage;

/**
 * @author Daniel Azarov
 */
public class ConfigureProblemSeverityMarkerResolution implements
		IMarkerResolution2, IJavaCompletionProposal {
	private static final int PREFERENCE_SIZE = 40;
	private static final String DOTS = "...";
	
	private String preferencePageId;
	private String preferenceKey;
	private String label;
	
	public ConfigureProblemSeverityMarkerResolution(String preferencePageId, String preferenceKey){
		this.preferencePageId = preferencePageId;
		this.preferenceKey = preferenceKey;
		String preferenceName = getPreferenceLabel();
		label = NLS.bind(CommonUIMessages.CONFIGURE_PROBLEM_SEVERITY, preferenceName);
	}

	public String getLabel() {
		return label;
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
		return label;
	}

	public Image getImage() {
		return JavaPlugin.getImageDescriptorRegistry().get(JavaPluginImages.DESC_ELCL_CONFIGURE_PROBLEM_SEVERITIES);
	}
	
	private WorkbenchPreferenceNode findPageNode() {
		final PreferenceManager preferenceManager = PlatformUI.getWorkbench()
				.getPreferenceManager();
		List nodes = preferenceManager.getElements(PreferenceManager.POST_ORDER);
		for (Iterator i = nodes.iterator(); i.hasNext();) {
			IPreferenceNode node = (IPreferenceNode) i.next();
			if (node.getId().equals(preferencePageId)) {
				return (WorkbenchPreferenceNode)node;
			}
		}
		return null;
	}
	
	private String getPreferenceLabel() {
		String label = "";
		WorkbenchPreferenceNode pageNode = findPageNode();
		if(pageNode != null){
			IPreferencePage page = pageNode.getPage();
			if(page == null){
				pageNode.createPage();
				
				page = pageNode.getPage();
				
				label = getLabel(page);
				
				pageNode.setPage(null);
				page.dispose();
			}else{
				label = getLabel(page);
			}
		}
		return label;
	}
	
	private String getLabel(IPreferencePage page){
		if(page instanceof SeverityPreferencePage){
			return cut(((SeverityPreferencePage)page).getLabel(preferenceKey));
		}
		return "";
	}
	
	private String cut(String label){
		if(label.length() > PREFERENCE_SIZE){
			return label.substring(0, PREFERENCE_SIZE-1)+DOTS;
		}else{
			if(label.endsWith(":")){
				return label.substring(0, label.length()-1);
			}
			return label;
		}
	}

	@Override
	public void apply(IDocument document) {
		run(null);
	}

	@Override
	public Point getSelection(IDocument document) {
		return null;
	}

	@Override
	public String getAdditionalProposalInfo() {
		return label;
	}

	@Override
	public String getDisplayString() {
		return label;
	}

	@Override
	public IContextInformation getContextInformation() {
		return null;
	}

	@Override
	public int getRelevance() {
		return 0;
	}
}
