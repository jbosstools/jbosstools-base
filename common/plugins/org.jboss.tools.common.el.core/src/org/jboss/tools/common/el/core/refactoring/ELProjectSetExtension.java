package org.jboss.tools.common.el.core.refactoring;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.el.core.ELCorePlugin;

public class ELProjectSetExtension {
	public static String EXTENSION_POINT = "org.jboss.tools.common.el.core.elProjectSet"; //$NON-NLS-1$

	String id;
	ProjectsSet searcher;

	public ELProjectSetExtension() {}

	public String getId() {
		return id;
	}

	public ProjectsSet getProjectSet() {
		return searcher;
	}

	static ELProjectSetExtension[] INSTANCES;

	public static ELProjectSetExtension[] getInstances() {
		if(INSTANCES != null) return INSTANCES;
		List<ELProjectSetExtension> list = new ArrayList<ELProjectSetExtension>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		IConfigurationElement[] es = point.getConfigurationElements();
		for (IConfigurationElement e: es) {
			ELProjectSetExtension n = new ELProjectSetExtension();
			n.id = e.getAttribute("id"); //$NON-NLS-1$
			try{
				n.searcher = (ProjectsSet)e.createExecutableExtension("projectset-class"); //$NON-NLS-1$
			}catch(CoreException ex){
				ELCorePlugin.getDefault().logError(ex);
			}
			list.add(n);
		}
		return INSTANCES = list.toArray(new ELProjectSetExtension[0]);
	}
}
