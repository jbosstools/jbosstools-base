package org.jboss.tools.common.model.filesystems.impl;

import java.util.ArrayList;
import java.util.List;

import org.jboss.tools.common.model.XModelObjectConstants;

public class TestJarAccess {
	int threadCount = 10;
	JarAccess acc = new JarAccess();
	
	List<String> errors = new ArrayList<String>();
	
	public TestJarAccess(String location) {
		acc.setLocation(location);
	}

	public void runAll() {
		Thread[] ts = new Thread[threadCount];
		for (int i = 0; i < threadCount; i++) {
			Runner r = new Runner(i);
			Thread t = new Thread(r);
			t.start();
			ts[i] = t;
		}
		boolean b = true;
		while(b) {
			b = false;
			for (int i = 0; i < threadCount; i++) {
				if(ts[i].isAlive()) b = true;
			}
			if(b) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}

	public List<String> getErrors() {
		errors.addAll(acc.getErrors());
		return errors;
	}

	class Runner implements Runnable {
		int id;
		
		public Runner(int id) {
			this.id = id;
		}
		public void run() {
			try {
				for (int i = 0; i < 100; i++) {
					readAllFiles("");
				}
			} catch (Exception e) {
				errors.add(e.getClass().getName() + ": " + e.getMessage());
			}
		}
		
	}

	void readAllFiles(String path) {
		String[] ps = acc.getChildren(path);
		String parentPath = (path.length() == 0) ? "" : path + XModelObjectConstants.SEPARATOR;
		if(ps != null) for (int i = 0; i < ps.length; i++) {
			boolean d = ps[i].endsWith(XModelObjectConstants.SEPARATOR);
			if(d) ps[i] = ps[i].substring(0, ps[i].length() - 1);
			if(d) {
				readAllFiles(parentPath + ps[i]);
			} else {
				acc.getContent(parentPath + ps[i]);
			}
		}
	}
	
}
