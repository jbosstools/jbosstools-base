package org.jboss.tools.common.model.project.ext;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileSystemsLoader;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.XModelObjectUtil;

/**
 * Monitors class path of project and loads seam components of it.
 *  
 * @author Viacheslav Kabanovich
 */
public abstract class AbstractClassPathMonitor<P> {
	protected XModel model = null;
	protected P project;
	
	protected List<String> paths = null;
	protected Map<IPath, String> paths2 = new HashMap<IPath, String>();
	
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
	}

	public abstract IProject getProjectResource();
	
	/**
	 * Returns true if class path was up-to-date.
	 * Otherwise, updates inner model and disables class loader.
	 * @return
	 */
	public boolean update() {
		List<String> newPaths = null;
		try {
			newPaths = EclipseResourceUtil.getClassPath(getProjectResource());
			List<String> jre = EclipseResourceUtil.getJREClassPath(getProjectResource());
			if(jre != null) newPaths.removeAll(jre);
		} catch (CoreException e) {
			//TODO
			ModelPlugin.getDefault().logError(e);
		} catch(IOException e) {
			ModelPlugin.getDefault().logError(e);			
		}
		if(paths == null && newPaths == null) return false;
		if((newPaths == null || paths == null) || (paths.size() != newPaths.size())) {
			paths = newPaths;
		} else { 
			boolean b = false;
			for (int i = 0; i < paths.size() && !b; i++) {
				if(!paths.get(i).equals(newPaths.get(i))) b = true;
			}
			if(!b) return false;
			paths = newPaths;
		}
		createMap();
		XModelObject object = model.getByPath("FileSystems"); //$NON-NLS-1$
		XModelObject[] fs = object.getChildren("FileSystemJar"); //$NON-NLS-1$
		Set<XModelObject> fss = new HashSet<XModelObject>();
		for (int i = 0; i < fs.length; i++) fss.add(fs[i]);
		
		for (int i = 0; i < paths.size(); i++) {
			String path = paths.get(i);
			if(!EclipseResourceUtil.isJar(path)) continue;
			String fileName = new File(path).getName();
			if(EclipseResourceUtil.SYSTEM_JAR_SET.contains(fileName)) continue;
			String jsname = "lib-" + fileName; //$NON-NLS-1$
			XModelObject o = model.getByPath("FileSystems").getChildByPath(jsname); //$NON-NLS-1$
			if(o != null) {
				fss.remove(o);
			} else {
				o = object.getModel().createModelObject("FileSystemJar", null); //$NON-NLS-1$
				o.setAttributeValue("name", jsname); //$NON-NLS-1$
				o.setAttributeValue("location", path); //$NON-NLS-1$
				o.set(FileSystemsLoader.IS_ADDED_TO_CLASSPATH, "true"); //$NON-NLS-1$
				object.addChild(o);
//				object.setModified(true);
			}			
		}
		
		for (XModelObject o: fss) {
			String path = XModelObjectUtil.expand(o.getAttributeValue("location"), o.getModel(), null); //$NON-NLS-1$
			if("true".equals(o.get(FileSystemsLoader.IS_ADDED_TO_CLASSPATH))) { //$NON-NLS-1$
				o.removeFromParent(); 
			} else if(!new File(path).exists()) {
				o.removeFromParent();
			}			
		}
		
		return true;
	}
	
	private void createMap() {
		paths2.clear();
		if(paths != null) {
			for (String p : paths) {
				paths2.put(new Path(p), p);
			}
		}
	}
	
	public void pathLoaded(IPath path) {
		String p = paths2.get(path);
		if(p != null) {
			processedPaths.add(p);
		}
	}
	
	public boolean hasPath(IPath path) {
		return paths2.get(path) != null;
	}

	public void clean() {
		paths = null;
		if(paths2 != null) paths2.clear();
		processedPaths.clear();
	}

}
