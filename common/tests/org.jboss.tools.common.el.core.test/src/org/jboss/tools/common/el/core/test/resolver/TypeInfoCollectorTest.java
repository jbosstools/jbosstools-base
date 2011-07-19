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
package org.jboss.tools.common.el.core.test.resolver;

import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberInfo;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.ResourcesUtils;

/**
 * @author Alexey Kazakov
 */
public class TypeInfoCollectorTest extends TestCase {

	protected static String PLUGIN_ID = "org.jboss.tools.common.el.core.test";

	IProject project1 = null;
	IProject project2 = null;

	@Override
	protected void setUp() throws Exception {
		project1 = ResourcesUtils.importProject(PLUGIN_ID, "/projects/JavaProject1");
		project1.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		JobUtils.waitForIdle();

		project2 = ResourcesUtils.importProject(PLUGIN_ID, "/projects/JavaProject2");
		project2.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		JobUtils.waitForIdle();
	}

	/**
	 * See https://issues.jboss.org/browse/JBIDE-9373
	 * @throws CoreException
	 */
	public void testTypeResolution() throws CoreException {
		IJavaProject jp = JavaCore.create(project2);
		IType bean = jp.findType("test.CollectionBean");
		TypeInfoCollector.TypeInfo typeInfo = new TypeInfoCollector.TypeInfo(bean, null, false);

		IField field = bean.getField("tests");
		TypeInfoCollector.FieldInfo fieldInfo = new TypeInfoCollector.FieldInfo(field, null, typeInfo, false);

		TypeInfoCollector collector = fieldInfo.getTypeCollector(false, false);
		MemberInfo info = getMethod(collector, "iterator");
		assertNotNull(info);
		info = getMethod(info.getTypeCollector(false, false), "next");
		assertNotNull(info);
		info = getMethod(info.getTypeCollector(false, false), "foo");
		assertNotNull(info);
		assertNotNull(info.getMemberType());
	}

	private MemberInfo getMethod(TypeInfoCollector collector, String name) {
		collector.collectInfo();
		List<MemberInfo> mts = collector.getMethods();
		for (MemberInfo info : mts) {
			if(name.equals(info.getName())) {
				return info;
			}
		}
		return null;
	}

	@Override
	public void tearDown() throws Exception {
		boolean saveAutoBuild = ResourcesUtils.setBuildAutomatically(false);
		JobUtils.waitForIdle();
		project1.delete(true, true, null);
		project2.delete(true, true, null);
		JobUtils.waitForIdle();
		ResourcesUtils.setBuildAutomatically(saveAutoBuild);
	}
}