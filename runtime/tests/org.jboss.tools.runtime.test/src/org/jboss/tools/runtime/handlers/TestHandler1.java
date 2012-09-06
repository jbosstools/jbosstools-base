package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetectorDelegate;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;

public class TestHandler1 extends AbstractRuntimeDetectorDelegate {

	private static TestHandler1 ME;
	
	public static TestHandler1 getInstance() {
		return ME;
	}
	
	public TestHandler1() {
		ME = this;
	}

	private ArrayList<String> initialized = new ArrayList<String>();
	public void reset() {
		initialized.clear();
	}
	private void addInit(String s) {
		initialized.add(s);
	}
	public String[] getInited() {
		return (String[]) initialized.toArray(new String[initialized.size()]);
	}
	
	@Override
	public RuntimeDefinition getRuntimeDefinition(File root,
			IProgressMonitor monitor) {
		if( root.isDirectory() && root.list().length == 0 ) {
			RuntimeDefinition def = new RuntimeDefinition(
					"Handler1 Folder " + root.getName(), "1.0", 
					"testHandler", root);
			return def;
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
