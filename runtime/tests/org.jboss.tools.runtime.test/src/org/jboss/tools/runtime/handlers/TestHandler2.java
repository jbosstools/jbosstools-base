/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetectorDelegate;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.test.RuntimeTestActivator;

public class TestHandler2 extends AbstractRuntimeDetectorDelegate {

	private static TestHandler2 ME;
	
	public static TestHandler2 getInstance() {
		return ME;
	}
	
	public TestHandler2() {
		ME = this;
		System.out.println("Inside TestHandler2 constructor: " + this.toString());
		try {
			throw new Exception();
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	private ArrayList<String> initialized = new ArrayList<String>();
	public void reset() {
		initialized.clear();
	}
	private void addInit(String s) {
		System.out.println("__  Adding s: " + s);
		initialized.add(s);
	}
	public String[] getInited() {
		System.out.println("__  getting inited: " + initialized);
		return (String[]) initialized.toArray(new String[initialized.size()]);
	}
	
	@Override
	public RuntimeDefinition getRuntimeDefinition(File root,
			IProgressMonitor monitor) {
		IPath rootPath = new Path(root.getAbsolutePath());
		IPath stateLoc = RuntimeTestActivator.getDefault().getStateLocation();
		if( stateLoc.isPrefixOf(rootPath)) {
			if( root.isDirectory() && root.list().length == 0 ) {
				RuntimeDefinition def = new RuntimeDefinition(
						"Handler1 Folder " + root.getName(), "1.0", 
						"testHandler", root);
				return def;
			}
		}
		return null;
	}

	@Override
	public void initializeRuntimes(List<RuntimeDefinition> runtimeDefinition) {
		for( int i = 0; i < runtimeDefinition.size(); i++ ) {
			String n = runtimeDefinition.get(i).getName();
			addInit(n);
		}
	}

	@Override
	public boolean exists(RuntimeDefinition runtimeDefinition) {
		return false;
	}

	@Override
	public void computeIncludedRuntimeDefinition(
			RuntimeDefinition runtimeDefinition) {
	}

	@Override
	public String getVersion(RuntimeDefinition runtimeDefinition) {
		return runtimeDefinition.getVersion();
	}

}
