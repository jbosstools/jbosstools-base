package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;

public class TestHandler1 extends AbstractRuntimeDetector {

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
	public RuntimeDefinition getServerDefinition(File root,
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
	public void initializeRuntimes(List<RuntimeDefinition> serverDefinitions) {
		for( int i = 0; i < serverDefinitions.size(); i++ ) {
			String n = serverDefinitions.get(i).getName();
			addInit(n);
		}
	}

	@Override
	public boolean exists(RuntimeDefinition serverDefinition) {
		// TODO Auto-generated method stub
		return false;
	}

}
