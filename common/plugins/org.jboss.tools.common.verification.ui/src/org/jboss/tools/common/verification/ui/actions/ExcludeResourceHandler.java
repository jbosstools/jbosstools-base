/*************************************************************************************
 * Copyright (c) 2008-2009 JBoss by Red Hat and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.verification.ui.actions;

import java.util.Iterator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.wst.validation.MutableProjectSettings;
import org.eclipse.wst.validation.ValidationFramework;
import org.eclipse.wst.validation.Validator;
import org.eclipse.wst.validation.internal.ValManager;
import org.eclipse.wst.validation.internal.ValPrefManagerProject;
import org.eclipse.wst.validation.internal.ValidatorMutable;
import org.eclipse.wst.validation.internal.ValManager.UseProjectPreferences;
import org.eclipse.wst.validation.internal.model.FilterGroup;
import org.eclipse.wst.validation.internal.model.FilterRule;
import org.eclipse.wst.validation.internal.model.ProjectPreferences;
import org.jboss.tools.common.verification.ui.Messages;
import org.jboss.tools.common.verification.ui.XStudioVerificationPlugin;

/**
* @author snjeza
* 
*/
public class ExcludeResourceHandler extends AbstractHandler {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
	 * .ExecutionEvent)
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelection(event);
		if (selection instanceof IStructuredSelection) {
			Iterator iter = ((IStructuredSelection) selection).iterator();
			while (iter.hasNext()) {
				Object element = iter.next();
				if (!(element instanceof IAdaptable)) {
					continue;
				}
				IResource resource = (IResource) ((IAdaptable) element)
						.getAdapter(IResource.class);
				if (resource != null) {
					excludeResource(resource);
				}
			}
		}
		return null;
	}

	private void excludeResource(IResource resource) {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		Shell shell = window.getShell();
		boolean exclude = MessageDialog.openQuestion(shell, Messages.ExcludeResourceHandler_Exclude_validation, NLS.bind(Messages.ExcludeResourceHandler_Are_you_sure_you_want_to_exclude_all_V2_validators,resource.getName()));
		if (!exclude) {
			return;
		}
		if (resource.getType() == IResource.PROJECT) {
			ValidationFramework.getDefault().disableValidation(resource);
		} else {
			IProject project = resource.getProject();
			MutableProjectSettings projectSettings = ValidationFramework.getDefault().getProjectSettings(project);
			projectSettings.setOverride(true);
			Validator[] validators  = ValManager.getDefault().getValidatorsConfiguredForProject(project, UseProjectPreferences.MustUse);
			ValidatorMutable[] changedValidator = new ValidatorMutable[validators.length];
			for (int i = 0; i < validators.length; i++) {
				ValidatorMutable validator = new ValidatorMutable(validators[i]);
				changedValidator[i] = validator;
				if (validator.isV2Validator()) {
					FilterGroup[] groups = validator.getGroups();
					FilterGroup excludeGroup = null;
					for (int j = 0; j < groups.length; j++) {
						if (groups[j].isExclude()) {
							excludeGroup = groups[j];
							break;
						}
					}
					if (excludeGroup == null) {
						excludeGroup = FilterGroup.create(true, new FilterRule[0]);
						validator.add(excludeGroup);
					}
					String pattern = resource.getProjectRelativePath().toString();
					int type = -1;
					if (resource.getType() == IResource.FILE) {
						type = FilterRule.File.FileTypeFile;
					} else if (resource.getType() == IResource.FOLDER) {
						type = FilterRule.File.FileTypeFolder;
					} 
					if (type != -1) {
						FilterRule rule = FilterRule.createFile(pattern, true, type);
						FilterGroup newGroup = FilterGroup.addRule(excludeGroup, rule);
						validator.replaceFilterGroup(excludeGroup, newGroup);
						
					}
				}
			}
			ValidationFramework.getDefault().applyChanges(projectSettings, true);			
			ProjectPreferences pp = new ProjectPreferences(project, projectSettings.getOverride(), projectSettings.getSuspend(), null);
			ValPrefManagerProject vpm = new ValPrefManagerProject(project);
			vpm.savePreferences(pp, changedValidator);
		}
		try {
			resource.setPersistentProperty(XStudioVerificationPlugin.RESOURCE_EXCLUDED, "true"); //$NON-NLS-1$
		} catch (CoreException ignore) {
			ignore.printStackTrace();
		}
	}

}
