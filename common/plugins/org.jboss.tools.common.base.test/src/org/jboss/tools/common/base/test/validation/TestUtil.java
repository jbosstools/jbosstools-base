package org.jboss.tools.common.base.test.validation;

import java.lang.reflect.InvocationTargetException;

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.wst.validation.ValidationFramework;
import org.eclipse.wst.validation.internal.ConfigurationManager;
import org.eclipse.wst.validation.internal.FilterUtil;
import org.eclipse.wst.validation.internal.InternalValidatorManager;
import org.eclipse.wst.validation.internal.ProjectConfiguration;
import org.eclipse.wst.validation.internal.RegistryConstants;
import org.eclipse.wst.validation.internal.ValManager;
import org.eclipse.wst.validation.internal.operations.EnabledValidatorsOperation;
import org.eclipse.wst.validation.internal.operations.ValidatorSubsetOperation;
import org.eclipse.wst.validation.internal.operations.WorkbenchReporter;
import org.eclipse.wst.validation.internal.plugin.ValidationPlugin;
import org.jboss.tools.common.validation.ValidatorManager;
import org.jboss.tools.test.util.JobUtils;

public class TestUtil {

	/**
	 * Wait for validation to be completed.
	 * Usage:
	 *       ValidatorManager.setStatus("Any status but ValidatorManager.SLEEPING");
	 *       ... // do some work here which will make Eclipse build the project
	 *       TestUtil.waitForValidation(project);
	 * @throws CoreException
	 */
	public static void _waitForValidation(IProject project) throws CoreException{

		project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
		ValidationFramework.getDefault().suspendAllValidation(false);
		try {
			new EnabledValidatorsOperation(project,false){
				public void run(IProgressMonitor progressMonitor) throws OperationCanceledException {
					try {
						// In order to check whether or not the monitor has been canceled, the monitor must not be null.
						if (progressMonitor == null)return;
						IProject project = getProject();
						if (ValidationFramework.getDefault().isSuspended(project))return;
						if (ValManager.getDefault().isDisabled(project))return;

						if (!areValidatorsEnabled()) {
							// save some processing time...
							return;
						}

						final WorkbenchReporter reporter = new WorkbenchReporter(getProject(), progressMonitor) {
							public void addMessage(org.eclipse.wst.validation.internal.provisional.core.IValidator validator, org.eclipse.wst.validation.internal.provisional.core.IMessage message) {
								super.addMessage(validator, message);
								System.out.println(message);
							};
						};

						try {
							// Periodically check if the user has canceled the operation
							checkCanceled(reporter);
							preValidate(reporter);
							validate(reporter);
						} catch (CoreException e) {
							ValidationPlugin.getPlugin().handleException(e);
						}
					} finally {
					}
				}		
			}.run(new NullProgressMonitor());
		} finally {
			ValidationFramework.getDefault().suspendAllValidation(true);
		}
	}

	public static void validate(IResource resource) throws CoreException {
		validate(resource.getProject(), new IResource[] {resource});
	}

	public static void validate(IProject project, IResource[] resources) throws CoreException {
		ValidationFramework.getDefault().suspendAllValidation(true);
		project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
		ValidationFramework.getDefault().suspendAllValidation(false);
		try {
			new IncrimantalValidatorOperation(project, resources).run(new NullProgressMonitor());
//			new EnabledIncrementalValidatorsOperation(project, resources).run(new NullProgressMonitor());
//			new ValidatorSubsetOperation(project,"java",resource,false).run(new NullProgressMonitor());
		} catch (OperationCanceledException e) {
			e.printStackTrace();
			TestCase.fail(e.getMessage());
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			TestCase.fail(e.getMessage());
		} finally {
			ValidationFramework.getDefault().suspendAllValidation(true);
		}
	}

	private static class IncrimantalValidatorOperation extends ValidatorSubsetOperation {
		public IncrimantalValidatorOperation(IProject project, Object[] changedResources) throws InvocationTargetException {
			super(project, shouldForce(changedResources), RegistryConstants.ATT_RULE_GROUP_DEFAULT, false);
			ProjectConfiguration prjp = ConfigurationManager.getManager().getProjectConfiguration(project);
			setEnabledValidators(InternalValidatorManager.wrapInSet(prjp.getEnabledIncrementalValidators(true)));
			setFileDeltas(FilterUtil.getFileDeltas(getEnabledValidators(), changedResources, false));
		}
	}

	public static boolean waitForValidation() throws CoreException{
		for (int i = 0; i < 50; i++) {
			if(ValidatorManager.getStatus().equals(ValidatorManager.SLEEPING)) {
				return true;
			}
			JobUtils.delay(100);
			JobUtils.waitForIdle();
		}
		return false;
	}
}