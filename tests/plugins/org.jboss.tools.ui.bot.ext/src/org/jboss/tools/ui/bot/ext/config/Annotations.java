package org.jboss.tools.ui.bot.ext.config;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;



public class Annotations {
	/**
	 * annotation which defines requirement of whole test Class
	 * by default all sub-annotations are optional and are disabled, please go through
	 * documentation of all items.
	 * <ul>
	 * <li>{@link SWTBotTestRequires#clearProjects()}</li>
	 * <li>{@link SWTBotTestRequires#clearWorkspace()}</li>
	 * <li>{@link SWTBotTestRequires#perspective()}</li>
	 * <li>{@link Server}</li>
	 * <li>{@link Seam}</li>
	 * </ul>
	 * @author lzoubek
	 *
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.TYPE)
	public @interface SWTBotTestRequires {	
		/**
		 * optionally require server
		 */
		Server server() default @Server( required = false );
		/**
		 * optionally require seam runtime
		 * @return
		 */
		Seam seam() default @Seam( required = false );
		/**
		 * optionally require ESB runtime
		 * @return
		 */
		ESB esb() default @ESB( required = false);
		/**
		 * optionally require JBPM runtime
		 * @return
		 */
		JBPM jbpm() default @JBPM ( required = false );
		/**
		 * name of perspective to run within
		 * @return
		 */		
		String perspective() default "";
		/**
		 * my default workspace is cleaned before test (attempt to close all shells and editors, closes 'Welcome' view), 
		 * setting this to false will disable this feature
		 * @return
		 */
		boolean clearWorkspace() default true;
		/**
		 * by default all projects are undeployed from pre-configured server & deleted before test runs
		 * setting this to false will disable this feature
		 * @return
		 */
		boolean clearProjects() default true;
		/**
		 * if you want your class to be run just once (among all suites and possibly multiple configurations)
		 * set this to true, default is false. 
		 * This is useful when testing against multiple configurations is not needed 
		 * or for test class which would last too long and would uselessly run more than once
		 * @return
		 */
		boolean runOnce() default false;
	}
	/**
	 * Server requirement, by default matches all server types and versions
	 * @author lzoubek
	 *
	 */
	@Retention(RetentionPolicy.RUNTIME)
	public @interface Server {
		/**
		 * true if Server is required (default)
		 * @return
		 */
		boolean required() default true;
		/**
		 * state (default Running)
		 * @return
		 */
		ServerState state() default ServerState.Running;
		/**
		 * server type to match (Default ALL)
		 * @return
		 */
		ServerType type() default ServerType.ALL;
		/**
		 * version of required server (use * for all versions) default *
		 * @return
		 */
		String version() default "*";
		/**
		 * defines operator to match server version, possible values (=,<,>=<=,>=,!=) default =
		 * @return
		 */
		String operator() default "=";	
	}
	/**
	 * 
	 * @author lzoubek@redhat.com
	 *
	 */
	@Retention(RetentionPolicy.RUNTIME)
	public @interface Seam {		
		/**
		 * true if Seam is required (default)
		 * @return
		 */
		boolean required() default true;
		/**
		 * version of required runtime (use * for all)
		 * @return
		 */
		String version() default "*";
		/**
		 * defines operator on runtime version, possible values (=,<,>=<=,>=,!=) default =
		 * @return
		 */
		String operator() default "=";
		
	}
	/**
	 * 
	 * @author lzoubek@redhat.com
	 *
	 */
	@Retention(RetentionPolicy.RUNTIME)
	public @interface ESB {
		/**
		 * true if ESB is required (default)
		 * @return
		 */
		boolean required() default true;
		/**
		 * version of required runtime (use * for all)
		 * @return
		 */
		String version() default "*";
		/**
		 * defines operator on runtime version, possible values (=,<,>=<=,>=,!=) default =
		 * @return
		 */
		String operator() default "=";
		
	}
	
	
	@Retention(RetentionPolicy.RUNTIME)
	public @interface JBPM {
		/**
		 * true if JBPM is required (default)
		 * @return
		 */
		boolean required() default true;
		/**
		 * version of require runtime (use * for all)
		 * @return
		 */
		String version() default "*";
		/**
		 * defines operator on runtime version, possible values (=,<,>=<=,>=,!=) default =
		 * @return
		 */
		String operator() default "=";
	}
	
	
	public enum ServerState {
		/**
		 * server is present, no matter if runs or not
		 */
		Present,
		/**
		 * server will run
		 */
		Running,
		/**
		 * server present, but not running
		 */
		NotRunning,
		/**
		 * server (and runtime) not present
		 */
		Disabled
	}
	public enum ServerType {
		/**
		 * SOA platform (includes EAP)
		 */
		SOA,
		/**
		 * EAP
		 */
		EAP, 
		/**
		 * Jboss community version
		 */
		JbossAS,
		/**
		 * EAP with portal
		 */
		EPP,
		/**
		 * all server types acceptable
		 */
		ALL
	}
}
