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
package org.jboss.tools.common.validation.test;

import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Map;

import junit.framework.TestCase;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFileState;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourceAttributes;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.jboss.tools.common.base.test.validation.ValidationExceptionLogger;
import org.jboss.tools.common.base.test.validation.ValidationExceptionTest;
import org.jboss.tools.common.validation.ContextValidationHelper;
import org.jboss.tools.common.validation.ValidatorManager;

/**
 * @author Alexey Kazakov
 */
public class SynchronizationTest extends TestCase {

	static final IProject CHANGED_PROJECT_1 = ResourcesPlugin.getWorkspace().getRoot().getProject("ChangedProject1"); // Fake project which is "changed" by user. Thread 1. 
	static final IProject PROJECT_A_1 = ResourcesPlugin.getWorkspace().getRoot().getProject("ProjectA1"); // Fake root project. Depends on ChangedProject1.
	static final IProject CHANGED_PROJECT_B_1 = ResourcesPlugin.getWorkspace().getRoot().getProject("ChangedProjectB1"); // Fake project which is "changed" by user. Thread 2. Depends on ChangedProject1
	static final IProject PROJECT_B_2 = ResourcesPlugin.getWorkspace().getRoot().getProject("ProjectB2"); // Fake root project. Depends on ChangedProjectB1.
	static final IProject PROJECT_B_A_1 = ResourcesPlugin.getWorkspace().getRoot().getProject("ProjectBA1"); // Fake root project. Depends on ChangedProjectB1.

	public void testSynchronizedValidatotion() throws Exception {
		ValidationExceptionLogger logger = new ValidationExceptionLogger();

		final ContextValidationHelper helperThread1 = new ContextValidationHelper();
		helperThread1.setProject(CHANGED_PROJECT_B_1);
		FakeFile changeFileProjectB1 = new FakeFile(CHANGED_PROJECT_B_1, "ChangeFileProjectB1.java");
		helperThread1.registerResource(changeFileProjectB1);
		helperThread1.setValidationFileURIs(new ArrayList<String>());
		final ValidatorManager managerThread1 = new ValidatorManager();

		final ContextValidationHelper helperThread2 = new ContextValidationHelper();
		helperThread2.setProject(CHANGED_PROJECT_1);
		FakeFile changeFileProject1 = new FakeFile(CHANGED_PROJECT_1, "ChangeFileProject1.java");
		helperThread2.registerResource(changeFileProject1);
		helperThread2.setValidationFileURIs(new ArrayList<String>());
		final ValidatorManager managerThread2 = new ValidatorManager();

		final ThreadHolder threadHolder = new ThreadHolder();
		for (int i = 1; i < 4; i++) {
			Runnable thread1 = new Runnable() {
				public void run() {
					try {
						managerThread1.validate(helperThread1, null);
					} catch (ValidationException e) {
						threadHolder.setException(e);
					} finally {
						threadHolder.finish();
					}
				}
			};
			Runnable thread2 = new Runnable() {
				public void run() {
					try {
						managerThread2.validate(helperThread2, null);
					} catch (ValidationException e) {
						threadHolder.setException(e);
					} finally {
						threadHolder.finish();
					}
				}
			};

			threadHolder.start();
			new Thread(thread1).start();
			threadHolder.start();
			new Thread(thread2).start();

			// Wait for all validators 
			while (!threadHolder.isReady()) {
				Thread.sleep(10);
			}
			if(threadHolder.getException()!=null) {
				throw new Exception(threadHolder.getException());
			}
			ValidationExceptionTest.assertExceptionsIsEmpty(logger);
			assertProjectValidation(i, PROJECT_A_1);
			assertProjectValidation(i, PROJECT_B_2);
			assertProjectValidation(i, PROJECT_B_A_1);
		}
	}

	private void assertProjectValidation(int attempt, IProject project) {
		assertTrue("Project " + project.getName() + " has not been validated. Attemp #" + attempt, TestSynchronizationValidator.isProjectValidated(project));
	}

	private static class ThreadHolder {
		private Exception exception;
		private int startedValidators;

		public synchronized void finish() {
			startedValidators--;
		}
		public synchronized void start() {
			startedValidators++;
		}
		public synchronized boolean isReady() {
			return startedValidators==0;
		}
		public synchronized Exception getException() {
			return exception;
		}
		public synchronized void setException(Exception exception) {
			this.exception = exception;
		}
	}

	private static class FakeFile implements IFile {
		private IProject project;
		private String name;
		public FakeFile(IProject project, String name) {
			this.project = project;
			this.name = name;
		}
		public boolean exists() {
			return true;
		}
		public String getFileExtension() {
			return name.substring(name.lastIndexOf('.'));
		}
		public IContainer getParent() {
			return project;
		}
		public IProject getProject() {
			return project;
		}
		public boolean isAccessible() {
			return true;
		}
		public void accept(IResourceProxyVisitor visitor, int memberFlags)	throws CoreException {}
		public void accept(IResourceVisitor visitor) throws CoreException {}
		public void accept(IResourceVisitor visitor, int depth,	boolean includePhantoms) throws CoreException {}
		public void accept(IResourceVisitor visitor, int depth, int memberFlags) throws CoreException {}
		public void clearHistory(IProgressMonitor monitor) throws CoreException {}
		public void copy(IPath destination, boolean force, IProgressMonitor monitor) throws CoreException {}		
		public void copy(IPath destination, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void copy(IProjectDescription description, boolean force, IProgressMonitor monitor) throws CoreException {}		
		public void copy(IProjectDescription description, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public IMarker createMarker(String type) throws CoreException {return null;}
		public IResourceProxy createProxy() {return null;}
		public void delete(boolean force, IProgressMonitor monitor)	throws CoreException {}
		public void delete(int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void deleteMarkers(String type, boolean includeSubtypes,	int depth) throws CoreException {}
		public IMarker findMarker(long id) throws CoreException {return null;}
		public IMarker[] findMarkers(String type, boolean includeSubtypes, int depth) throws CoreException {return null;}
		public int findMaxProblemSeverity(String type, boolean includeSubtypes,	int depth) throws CoreException {return 0;}
		public long getLocalTimeStamp() {return 0;}
		public IPath getLocation() {return null;}
		public URI getLocationURI() {return null;}
		public IMarker getMarker(long id) {return null;}
		public long getModificationStamp() {return 0;}
		public IPathVariableManager getPathVariableManager() {return null;}
		public Map<QualifiedName, String> getPersistentProperties()	throws CoreException {return null;}
		public String getPersistentProperty(QualifiedName key) throws CoreException {return null;}
		public IPath getProjectRelativePath() {return null;}
		public IPath getRawLocation() {return null;}
		public URI getRawLocationURI() {return null;}
		public ResourceAttributes getResourceAttributes() {return null;}
		public Map<QualifiedName, Object> getSessionProperties() throws CoreException {return null;}
		public Object getSessionProperty(QualifiedName key)	throws CoreException {return null;}
		public int getType() {return 0;}
		public IWorkspace getWorkspace() {return null;}
		public boolean isDerived() {return false;}
		public boolean isDerived(int options) {return false;}
		public boolean isHidden() {return false;}
		public boolean isHidden(int options) {return false;}
		public boolean isLinked() {return false;}
		public boolean isVirtual() {return false;}
		public boolean isLinked(int options) {return false;}
		public boolean isLocal(int depth) {return false;}
		public boolean isPhantom() {return false;}
		public boolean isSynchronized(int depth) {return false;}
		public boolean isTeamPrivateMember() {return false;}
		public boolean isTeamPrivateMember(int options) {return false;}
		public void move(IPath destination, boolean force, IProgressMonitor monitor) throws CoreException {}
		public void move(IPath destination, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void move(IProjectDescription description, boolean force, boolean keepHistory, IProgressMonitor monitor) throws CoreException {}		
		public void move(IProjectDescription description, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void refreshLocal(int depth, IProgressMonitor monitor) throws CoreException {}
		public void revertModificationStamp(long value) throws CoreException {}
		public void setDerived(boolean isDerived) throws CoreException {}
		public void setDerived(boolean isDerived, IProgressMonitor monitor) throws CoreException {}
		public void setHidden(boolean isHidden) throws CoreException {}
		public void setLocal(boolean flag, int depth, IProgressMonitor monitor) throws CoreException {}
		public long setLocalTimeStamp(long value) throws CoreException {return 0;}
		public void setPersistentProperty(QualifiedName key, String value) throws CoreException {}
		public void setReadOnly(boolean readOnly) {}
		public void setResourceAttributes(ResourceAttributes attributes) throws CoreException {}
		public void setSessionProperty(QualifiedName key, Object value) throws CoreException {}
		public void setTeamPrivateMember(boolean isTeamPrivate)	throws CoreException {}
		public void touch(IProgressMonitor monitor) throws CoreException {}
		public Object getAdapter(Class adapter) {return null;}
		public boolean contains(ISchedulingRule rule) {return false;}
		public boolean isConflicting(ISchedulingRule rule) {return false;}
		public void appendContents(InputStream source, boolean force, boolean keepHistory, IProgressMonitor monitor) throws CoreException {}
		public void appendContents(InputStream source, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void create(InputStream source, boolean force, IProgressMonitor monitor) throws CoreException {}
		public void create(InputStream source, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void createLink(IPath localLocation, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void createLink(URI location, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void delete(boolean force, boolean keepHistory, IProgressMonitor monitor) throws CoreException {}
		public String getCharset() throws CoreException {return null;}
		public String getCharset(boolean checkImplicit) throws CoreException {return null;}
		public String getCharsetFor(Reader reader) throws CoreException {return null;}
		public IContentDescription getContentDescription() throws CoreException {return null;}
		public InputStream getContents() throws CoreException {return null;}
		public InputStream getContents(boolean force) throws CoreException {return null;}
		public int getEncoding() throws CoreException {return 0;}
		public IPath getFullPath() {return null;}
		public IFileState[] getHistory(IProgressMonitor monitor) throws CoreException {return null;}
		public String getName() {return null;}
		public boolean isReadOnly() {return false;}
		public void move(IPath destination, boolean force, boolean keepHistory, IProgressMonitor monitor) throws CoreException {}
		public void setCharset(String newCharset) throws CoreException {}
		public void setCharset(String newCharset, IProgressMonitor monitor) throws CoreException {}
		public void setContents(InputStream source, boolean force, boolean keepHistory, IProgressMonitor monitor) throws CoreException {}
		public void setContents(IFileState source, boolean force, boolean keepHistory, IProgressMonitor monitor) throws CoreException {}
		public void setContents(InputStream source, int updateFlags, IProgressMonitor monitor) throws CoreException {}
		public void setContents(IFileState source, int updateFlags, IProgressMonitor monitor) throws CoreException {}
	}
}