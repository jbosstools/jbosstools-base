/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.checkup;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingDeque;

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.osgi.internal.messages.Msg;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.internal.views.log.LogSession;
import org.eclipse.ui.internal.views.log.TailInputStream;
import org.eclipse.ui.progress.UIJob;
import org.osgi.framework.BundleException;
import org.osgi.framework.Constants;
import org.osgi.service.prefs.BackingStoreException;

public class JVMProblemDetector implements IStartup, ILogListener{
	private static final String PREFERENCE_NAME="org.jboss.tools.foundation.checkup.JVMProblemDetector_NOT_TO_SHOW"; //$NON-NLS-1$
	
	private static final String STRING_1 = BundleException.class.getCanonicalName()+": "+Msg.Module_ResolveError; //$NON-NLS-1$
	private static final String STRING_2 = "-> "+Constants.BUNDLE_SYMBOLICNAME+":"; //$NON-NLS-1$
	private static final String STRING_3 = Msg.ModuleResolutionReport_UnresolvedReq+Constants.REQUIRE_CAPABILITY + ": osgi.ee; filter:=\"(&(osgi.ee="; //$NON-NLS-1$
	private static final String STRING_4 = ")(version="; //$NON-NLS-1$
	
	private static final long MAX_LENGTH = 1024 * 1024;
	
	// possible values of eclipse.application system property
	// need to cover:
	// "org.eclipse.pde.junit.runtime.uitestapplication"
	// "org.eclipse.pde.junit.runtime.coretestapplication"
	
	private static final String JUNIT_APPLICATION_PREFIX = "org.eclipse.pde.junit.runtime."; //$NON-NLS-1$
	
	// need to cover
	// "org.eclipse.tycho.surefire.osgibooter.uitest32"
	// "org.eclipse.tycho.surefire.osgibooter.uitest"
	// "org.eclipse.tycho.surefire.osgibooter.headlesstest"
	
	private static final String TYCHO_APPLICATION_PREFIX = "org.eclipse.tycho.surefire.osgibooter."; //$NON-NLS-1$
	
	// in order to collect more data to show we do not show the warning dialog immediately 
	// time to wait before show the warning dialog in milliseconds
	private static final long WAIT_TIME_AFTER_READING_LOG = 7000;
	private static final long WAIT_TIME_AFTER_EVENT = 3000;
	
	private UnresolvedStructure structure = new UnresolvedStructure();
	
	private LinkedBlockingDeque<String> queue = new LinkedBlockingDeque<String>();
	
	private String javaVersion = null;
	
	private boolean testEnvironment = false;
	
	private static JVMProblemDetector instance = null;
	
	public JVMProblemDetector(){
		super();
		instance = this;
	}
	
	private JVMProblemDetectorJob job = null;
	
	public void earlyStartup() {
		if(isAllowedToShow()){
			javaVersion = System.getProperty("java.version"); //$NON-NLS-1$
			
			// listen log
			Platform.addLogListener(JVMProblemDetector.this);

			// read error log
			
			readLogFile();

			// start job which read the queue
			job = new JVMProblemDetectorJob();
			job.schedule();
		}
	}
	
	public static void cancelJob(){
		if(instance != null){
			if(instance.job != null){
				instance.job.cancel();
				instance.job = null;
			}
		instance = null;
		}
	}
	
	class JVMProblemDetectorJob extends Job{

		public JVMProblemDetectorJob() {
			super(JVMProblemDetectorMessages.JOB_TITLE);
			setSystem(true);
			setPriority(LONG);
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			readQueue();
			
			Status status = new Status(Status.OK, FoundationCheckupPlugin.PLUGIN_ID, "");
			return status;
		}
		
	}
	
	/**
	 * for test purpose 
	 * @return
	 */
	public static UnresolvedStructure getUnresolvedStructure(){
		if(instance == null){
			instance = new JVMProblemDetector();
		}
		return instance.structure;
	}
	
	private void checkTestEnvironment(){
		String eclipseApplication = System.getProperty("eclipse.application"); //$NON-NLS-1$
		if(eclipseApplication != null && (eclipseApplication.startsWith(JUNIT_APPLICATION_PREFIX) ||
				eclipseApplication.startsWith(TYCHO_APPLICATION_PREFIX))){
			testEnvironment = true;
		}
	}

	private void readLogFile() {
		File logFile = Platform.getLogFileLocation().toFile();
		if (logFile == null || !logFile.exists()){
			return;
		}
			
		BufferedReader reader = null;
		try{
			reader = new BufferedReader(new InputStreamReader(new TailInputStream(logFile, MAX_LENGTH), "UTF-8"));  //$NON-NLS-1$
			while (true) {
				String line0 = reader.readLine();
				if (line0 == null)
					break;
				String line = line0.trim();
				
				scanLine(line);
			}
		} catch (FileNotFoundException e) {
			FoundationCheckupPlugin.logError(e);
		} catch (IOException e) {
			FoundationCheckupPlugin.logError(e);
		}finally{
			try {
				if(reader != null){
					reader.close();
				}
			} catch (IOException e) {
				// do nothing
			}
		}
		
		structure.report(WAIT_TIME_AFTER_READING_LOG);
	}
	
	private void readQueue(){
		
		try{
			while(isAllowedToShow() && !queue.isEmpty()){
				String message = queue.take();
				BufferedReader reader = null;
				try{
					reader = new BufferedReader(new StringReader(message)); 
					while (true) {
						String line0 = reader.readLine();
						if (line0 == null)
							break;
						String line = line0.trim();
						scanLine(line);
					}
				}finally{
					if(reader != null){
						reader.close();
					}
				}
				structure.report(WAIT_TIME_AFTER_EVENT);
			}
		} catch (IOException e){
			FoundationCheckupPlugin.logError(e);
		} catch (InterruptedException e) {
			// do nothing
		}
	}
	
	// temporary storage
	private List<String> moduleNameList = new ArrayList<String>();
	private String currentModuleName = null;

	private void scanLine(String line){
		if (line.startsWith(LogSession.SESSION)) {
			// clear previous data
			// call from NOT UI Thread
			structure.clear();
			moduleNameList.clear();
			currentModuleName = null;
		} else if(line.startsWith(STRING_1)){
			// parse unresolved module
			int position = line.indexOf("[");
			String unresolvedModule;
			if(position > 0){
				unresolvedModule = line.substring(STRING_1.length(), position).trim();
			}else{
				unresolvedModule = line.substring(STRING_1.length()).trim();
			}
			
			moduleNameList.clear();
			currentModuleName = unresolvedModule;
		} else if(line.startsWith(STRING_2)){
			// parse unresolved module
			int position = line.indexOf(";");
			String unresolvedModule;
			if(position > 0){
				unresolvedModule = line.substring(STRING_2.length(), position).trim();
			}else{
				unresolvedModule = line.substring(STRING_2.length()).trim();
			}
			
			if(currentModuleName != null && !moduleNameList.contains(currentModuleName)){
				moduleNameList.add(currentModuleName);
			}
			currentModuleName = unresolvedModule;
		} else if(line.startsWith(STRING_3)){
			// parse Java name and version
			int position = line.indexOf(STRING_4);
			if(position > 0){
				int endPosition = line.indexOf(")", position+STRING_4.length());
				
				String javaName = line.substring(STRING_3.length(), position).trim();
				String javaVersion;
				if(endPosition > 0){
					javaVersion = line.substring(position+STRING_4.length(), endPosition).trim();
				}else{
					javaVersion = line.substring(position+STRING_4.length()).trim();
				}
				// call from NOT UI Thread
				// store unresolved module
				structure.addRequieredJava(currentModuleName, moduleNameList, javaName, javaVersion);
			}
		}

	}
	
	private boolean isAllowedToShow(){
		return InstanceScope.INSTANCE.getNode(FoundationCheckupPlugin.PLUGIN_ID).getBoolean(PREFERENCE_NAME, true) || testEnvironment;
	}
	
	private void setAllowedToShow(boolean allowedToShow){
		if(!allowedToShow){
			Platform.removeLogListener(JVMProblemDetector.this);
		}
		IEclipsePreferences ep = InstanceScope.INSTANCE.getNode(FoundationCheckupPlugin.PLUGIN_ID);
		try {
			ep.putBoolean(PREFERENCE_NAME, allowedToShow);
			ep.flush();
		} catch (BackingStoreException e) {
			FoundationCheckupPlugin.logError(e);
		}
	}
	
	private void processStatus(IStatus status){
		Throwable exception = status.getException();
		String message = status.getMessage();
		if(message != null){
			try {
				queue.put(message);
			} catch (InterruptedException e) {
				// do nothing
			}
		}
		if(exception != null){
			message = exception.getMessage();
			if(message != null){
				try {
					queue.put(message);
				} catch (InterruptedException e) {
					// do nothing
				}
			}
			for(IStatus child : status.getChildren()){
				processStatus(child);
			}
		}
	}

	public void logging(IStatus status, String plugin) {
		processStatus(status);
		if(job != null){
			job.schedule();
		}
	}
	
	class ReportJob extends UIJob{
		
		public ReportJob() {
			super(JVMProblemDetectorMessages.SHOW_WARNING_DIALOG_JOB_TITLE);
		}

		public IStatus runInUIThread(IProgressMonitor monitor) {
			if(!JVMProblemDialog.showing){
				
				// call from UI Thread
				List<UnresolvedModule> modules = structure.getUnresolvedModules();
				
				JVMProblemDialog dialog = new JVMProblemDialog(Display.getDefault().getActiveShell(), modules, javaVersion);
				dialog.open();
				setAllowedToShow(dialog.showNextTime());
			}
			Status status = new Status(Status.OK, FoundationCheckupPlugin.PLUGIN_ID, "");
			return status;
		}
	}

	public class UnresolvedStructure{
		private List<UnresolvedModule> unresolvedModuleList = new ArrayList<UnresolvedModule>();
		
		/**
		 * returns copy of list of unresolved module
		 * supposed to be called from UI Thread
		 * @return
		 */
		public List<UnresolvedModule> getUnresolvedModules(){
			synchronized(unresolvedModuleList){
				List<UnresolvedModule> list = new ArrayList<UnresolvedModule>(unresolvedModuleList);
				if(!testEnvironment){
					unresolvedModuleList.clear();
				}
				return list;
			}
		}
		

		public boolean isNeedReport(){
			if(testEnvironment){
				return false;
			}
			synchronized(unresolvedModuleList){
				return unresolvedModuleList.size() > 0;
			}
		}
		
		public void clear(){
			synchronized(unresolvedModuleList){
				unresolvedModuleList.clear();
			}
		}
		
		
		public void addRequieredJava(String moduleName, List<String> moduleNameList, String javaName, String javaVersion){
			if(moduleName == null){
				return;
			}
			
			synchronized(unresolvedModuleList){
				UnresolvedModule unresolvedModule = new UnresolvedModule(moduleName, javaName, javaVersion);
				
				if(unresolvedModuleList.contains(unresolvedModule)){
					for(UnresolvedModule module : unresolvedModuleList){
						if(module.equals(unresolvedModule)){
							unresolvedModule = module;
							break;
						}
					}
				}else{
					unresolvedModuleList.add(unresolvedModule);
				}
				
				DependantList dependantList = unresolvedModule.getDependantList();
				for(String name : moduleNameList){
					Dependant dep = new Dependant(unresolvedModule, name);
					dependantList.add(dep);
				}
				
				moduleNameList.clear();
			}
		}
		
		public void report(long time){
			if(isNeedReport()){
				// show dialog window
				ReportJob job = new ReportJob();
				job.schedule(time);
			}
		}
	}
	
	public class UnresolvedModule{
		private String name;
		private String javaName;
		private String javaVersion;
		
		private DependantList list;
		
		public UnresolvedModule(String name, String javaName, String javaVersion){
			this.name = name;
			this.javaName = javaName;
			this.javaVersion = javaVersion;
			list = new DependantList(this);
		}
		
		public DependantList getDependantList(){
			return list;
		}
		
		public String toString(){
			return NLS.bind(JVMProblemDetectorMessages.UNRESOLVED_METHOD_LABEL, new Object[]{name, javaName, javaVersion});
		}

		public boolean equals(Object o) {
			if(o instanceof UnresolvedModule){
				return o.toString().equals(toString());
			}
			return super.equals(o);
		}

		public int hashCode() {
			return toString().hashCode();
		}
	}
	
	public class DependantList{
		private UnresolvedModule module;
		// list of Dependant
		private List<Dependant> dependants = new ArrayList<Dependant>();
		
		public DependantList(UnresolvedModule module){
			this.module = module;
		}
		
		public UnresolvedModule getUnresolvedModule(){
			return module;
		}
		
		public String toString(){
			return module.toString()+" "+JVMProblemDetectorMessages.DEPENDANT_MODULES;
		}
		
		public List<Dependant> getDependants(){
			return dependants;
		}
		
		
		public void add(Dependant dependant){
			synchronized(dependants){
				if(!dependants.contains(dependant)){
					dependants.add(dependant);
				}
			}
		}
		
		public boolean equals(Object o) {
			if(o instanceof Dependant){
				return o.toString().equals(toString());
			}
			return super.equals(o);
		}
		
		public int hashCode() {
			return toString().hashCode();
		}
	}
	
	public class Dependant{
		private UnresolvedModule module;
		private String name;
		
		public Dependant(UnresolvedModule module, String name){
			this.name = name;
			this.module = module;
		}
		
		public UnresolvedModule getParent(){
			return module;
		}
		
		public String toString(){
			return name;
		}
		
		public boolean equals(Object o) {
			if(o instanceof Dependant){
				return o.toString().equals(toString());
			}
			return super.equals(o);
		}
		
		public int hashCode() {
			return toString().hashCode();
		}
	}
}
