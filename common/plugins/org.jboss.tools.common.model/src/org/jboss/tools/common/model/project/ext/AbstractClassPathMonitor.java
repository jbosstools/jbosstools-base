package org.jboss.tools.common.model.project.ext;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.impl.Libs;
import org.jboss.tools.common.model.filesystems.impl.LibsListener;

/**
 * Monitors class path of project and loads seam components of it.
 *  
 * @author Viacheslav Kabanovich
 */
public abstract class AbstractClassPathMonitor<P> implements LibsListener {
	protected XModel model = null;
	protected P project;
	
	protected List<String> paths = new ArrayList<String>();
	private boolean loaded = false;

	protected Map<IPath, String> paths2 = new HashMap<IPath, String>();
	boolean libsModified = false;
	
	protected Set<String> processedPaths = new HashSet<String>();

	public AbstractClassPathMonitor() {
	}

	public P getProject() {
		return project;
	}

	/**
	 * Initialization of inner model.
	 */
	public void init() {
		if(model == null) return;
		Libs libs = FileSystemsHelper.getLibs(model);
		if(libs != null) libs.addListener(this);
	}

	public abstract IProject getProjectResource();
	
	/**
	 * Returns true if class path was up-to-date.
	 * Otherwise, updates inner model and disables class loader.
	 * @return
	 */
	public synchronized boolean update() {
		Libs libs = FileSystemsHelper.getLibs(model);
		if(libs == null) {
			return false;
		}
		libs.update();
		List<String> newPaths = libs.getPaths();
		boolean result = libsModified || !loaded;
		if(newPaths != null) {
			paths = newPaths;
			loaded = true;
		} else {
			paths = new ArrayList<String>();
			loaded = false;
		}
		if(result) {
			paths2.clear();
			paths2.putAll(libs.getPathsAsMap());
		}
		libsModified = false;
		return result;
	}
	
	public void pathLoaded(IPath path) {
		String p = paths2.get(path);
		if(p != null) synchronized (processedPaths) {
			processedPaths.add(p);
		}
	}
	
	public boolean hasPath(IPath path) {
		return paths2.get(path) != null;
	}

	public void clean() {
		paths = new ArrayList<String>();
		loaded = false;
		if(paths2 != null) paths2.clear();
		synchronized (processedPaths) {
			processedPaths.clear();
		}
	}

	protected List<String> syncProcessedPaths() {
		ArrayList<String> removed = new ArrayList<String>();
		synchronized (processedPaths) {
			Iterator<String> it = processedPaths.iterator();
			while(it.hasNext()) {
				String p = it.next();
				if(paths.contains(p)) continue;
				removed.add(p);
				it.remove();
			}
		}
		return removed;
	}

	protected boolean requestForLoad(String p) {
		synchronized (processedPaths) {
			return processedPaths.add(p);
		}
	}

	public void pathsChanged(List<String> paths) {
		synchronized (this) {
			libsModified = true;
		}
	}

}
