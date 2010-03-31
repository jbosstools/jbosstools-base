 /*******************************************************************************
  * Copyright (c) 2007 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.verification.vrules.core.resources;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.eclipse.wst.validation.internal.operations.IWorkbenchContext;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.eclipse.wst.validation.internal.provisional.core.IValidatorJob;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VModel;
import org.jboss.tools.common.verification.vrules.VObject;
import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.vrules.VTask;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;

/**
 * This Manager invokes all dependent validators that should be invoked in one job.
 * We need this one because wst validation framework does not let us invoke
 * dependent validators in the same job.
 * @author Alexey Kazakov
 */
public class CommonValidator implements IValidatorJob {

	private static Set<IProject> validatingProjects = new HashSet<IProject>(); 

	public CommonValidator() {
		super();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidatorJob#getSchedulingRule(org.eclipse.wst.validation.internal.provisional.core.IValidationContext)
	 */
	public ISchedulingRule getSchedulingRule(IValidationContext helper) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidatorJob#validateInJob(org.eclipse.wst.validation.internal.provisional.core.IValidationContext, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public IStatus validateInJob(IValidationContext helper, IReporter reporter)	throws ValidationException {
		IWorkbenchContext validationHelper = (IWorkbenchContext)helper;
		IProject project = validationHelper.getProject();
		String[] uris = validationHelper.getURIs();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		if(project == null && uris != null && uris.length != 0) {
			for (String s: uris) {
				IProject f = root.getProject(new Path(s).segment(0));
				if(f != null && f.exists()) {
					project = f;
					break;
				}
			}
		}
		
		if(project == null) {
			return OK_STATUS;
		}
		IModelNature n = EclipseResourceUtil.getModelNature(project);
		XModel model = n == null ? null : n.getModel();
		if(model == null || VHelper.getManager() == null) {
			return OK_STATUS;
		}
		VModel vmodel = VModelFactory.getModel(model);
		if(vmodel == null) {
			return OK_STATUS;
		}
		if(uris == null || uris.length == 0) {
			XModelObject object = FileSystemsHelper.getWebInf(model);
			if(object == null) {
				return OK_STATUS;
			}
			VObject vobject = vmodel.getObjectByPath(object.getPath());
			VRule[] rules = VHelper.getRules(VHelper.getManager(), vobject);
			if(rules == null) return OK_STATUS;
			VTask task = VHelper.getManager().createTask(vobject);
			VTaskListenerImpl listener = new VTaskListenerImpl();
			listener.setModel(model);
			listener.setTask(task);
			listener.setSignificance(VHelper.getManager().getMinSignificance());
			task.addTaskListener(listener);
			task.run();
			task.removeTaskListener(listener);
			return OK_STATUS;
		}
		List<IFile> files = new ArrayList<IFile>();
		for (String uri: uris) {
			IFile f = root.getFile(new Path(uri));
			if(f != null && f.exists() && f.getProject() == project) {
				files.add(f);
			}
		}
		if(files.isEmpty()) {
			return OK_STATUS;
		}
		IStatus status = OK_STATUS;
		for (IFile file: files) {
			XModelObject object = EclipseResourceUtil.getObjectByResource(file);
			if(object == null) continue;
			VObject vobject = vmodel.getObjectByPath(object.getPath());
			if(vobject == null) continue;
			VRule[] rules = VHelper.getRules(VHelper.getManager(), vobject);
			if(rules == null) continue;
			VTask task = VHelper.getManager().createTask(vobject);
			VTaskListenerImpl listener = new VTaskListenerImpl();
			listener.setModel(model);
			listener.setTask(task);
			listener.setSignificance(VHelper.getManager().getMinSignificance());
			task.addTaskListener(listener);
			task.run();
			task.removeTaskListener(listener);
		}
		return status;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidator#cleanup(org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void cleanup(IReporter reporter) {
		reporter = null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.provisional.core.IValidator#validate(org.eclipse.wst.validation.internal.provisional.core.IValidationContext, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void validate(IValidationContext helper, IReporter reporter)	throws ValidationException {
		validateInJob(helper, reporter);
	}
}